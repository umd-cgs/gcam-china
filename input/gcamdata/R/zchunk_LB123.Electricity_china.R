#' module_gcamchina_LB123.Electricity
#'
#' Electricity sector inputs and outputs, and electricity ownuse.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L123.in_EJ_province_elec_F}, \code{L123.out_EJ_province_elec_F}, \code{L123.in_EJ_province_ownuse_elec}, \code{L123.out_EJ_province_ownuse_elec}. The corresponding file in the
#' original data system was \code{LB123.Electricity.R} (gcam-china level1).
#' @details Compute electricity sector inputs and outputs, and electricity ownuse.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YanLiu August 2018
module_gcamchina_LB123.Electricity <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/CPSY_GWh_province_F_elec_out",
             FILE = "gcam-china/province_names_mappings",
             "L123.in_EJ_R_elec_F_Yh",
             "L123.out_EJ_R_elec_F_Yh",
             "L126.in_EJ_R_elecownuse_F_Yh",
             "L126.out_EJ_R_elecownuse_F_Yh",
             "L101.inNBS_Mtce_province_S_F",
             "L132.out_EJ_province_indchp_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L123.in_EJ_province_elec_F",
             "L123.out_EJ_province_elec_F",
             "L123.in_EJ_province_ownuse_elec",
             "L123.out_EJ_province_ownuse_elec"))
  } else if(command == driver.MAKE) {

    # =============================================================================
    GCAM_region_ID <- other <- row <- oil <- natural.gas <- coal<- coal.adj <- wind <- wind.adj <-
      nuclear<- nuc.adj <- solar <- solar.adj <- biomass <- fuel <- value <- province  <- sector <- year <-
      pct <- each <- Sum <- value.x <- value.y <- sector.x <- sector.y <- fuel.x <- fuel.y <- NULL     # silence package check.

    all_data <- list(...)[[1]]


    # -----------------------------------------------------------------------------
    # 1.Load required inputs
    province_names_mappings     <- get_data(all_data, "gcam-china/province_names_mappings")
    CPSY_GWh_province_F_elec_out <- get_data(all_data, "gcam-china/CPSY_GWh_province_F_elec_out")
    L101.inNBS_Mtce_province_S_F <- get_data(all_data, "L101.inNBS_Mtce_province_S_F")
    L123.in_EJ_R_elec_F_Yh <- get_data(all_data, "L123.in_EJ_R_elec_F_Yh")
    L123.out_EJ_R_elec_F_Yh <- get_data(all_data, "L123.out_EJ_R_elec_F_Yh")
    L126.in_EJ_R_elecownuse_F_Yh <- get_data(all_data, "L126.in_EJ_R_elecownuse_F_Yh")
    L126.out_EJ_R_elecownuse_F_Yh <- get_data(all_data, "L126.out_EJ_R_elecownuse_F_Yh")
    L132.out_EJ_province_indchp_F <- get_data(all_data, "L132.out_EJ_province_indchp_F")

    # -----------------------------------------------------------------------------
    # 2.perform computations
    XZ_ELEC_REALLOC_YEAR <- 2010

    CPSY_GWh_province_F_elec_out %>%
      map_province_name(province_names_mappings, "province", TRUE) %>%
      # In Tibet, electricity in 'other' will be split between oil, gas, solar, and geothermal in 2010. For prior years
      # it will be moved into coal.
      mutate(oil = 0,
             natural.gas = 0,
             oil = replace(oil, province == "XZ" & year == XZ_ELEC_REALLOC_YEAR, other[province == "XZ" & year == XZ_ELEC_REALLOC_YEAR] / 4),
             natural.gas = replace(natural.gas, province == "XZ" & year == XZ_ELEC_REALLOC_YEAR, other[province == "XZ" & year == XZ_ELEC_REALLOC_YEAR] / 4),
             coal = coal + coal.adj,
             wind = wind + wind.adj,
             nuclear = nuclear + nuc.adj,
             solar = solar + solar.adj) %>%
      select(-other, -row, -coal.adj, -wind.adj, -nuc.adj, -solar.adj) %>%
      # TODO: what about biomass?  Just use coal shares for now
      mutate(biomass = coal) %>%
      gather(fuel, value, -province, -year) %>%
      # remove a few negative values, very small (order of 10^-15) in geothermal) allocated from "other"
      filter(value >= 0) %>%
      complete(nesting(province, fuel), year = gcamchina.ELEC_HISTORICAL_YEARS) %>%
      arrange(province, fuel, year) %>%
      group_by(province, fuel) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup %>%
      mutate(fuel = as.character(fuel)) ->
      L123.GWh_province_F_elec_out

    # Oil and gas does not exist in the CPSY so we will use the CESY for those
    # Note these are in terms of energy in.  Since we are assuming constant efficiencies for all provinces
    # mixing these with the elec_out will be fine for apportioning to provinces
    L101.inNBS_Mtce_province_S_F %>%
      filter(sector == "electricity", fuel %in% c("refined liquids", "gas")) %>%
      replace_na(list(value = 0))  %>%
      select(-sector) %>%
      bind_rows(L123.GWh_province_F_elec_out)  %>%
      # In Tibet, refined liquids inherits oil and gas inherits natural gas
      mutate(fuel = replace(fuel, fuel == "oil" & province == "XZ", "refined liquids"),
             fuel = replace(fuel, fuel == "natural.gas" & province == "XZ", "gas"),
             # In Tibet our oil and gas numbers are in electricity out and they need to be energy in in Mtce. This is a total hack but
             # just divide by the efficiency. Gas is 0.55 and oil is about 0.35
             value =  replace(value, province == "XZ" & fuel == "refined liquids",
                              value[province == "XZ" & fuel == "refined liquids"] * 100 * CONV_GWH_EJ * CONV_EJ_MTCE / -0.35),
             value =  replace(value, province == "XZ" & fuel == "gas",
                              value[province == "XZ" & fuel == "gas"] * 100 * CONV_GWH_EJ * CONV_EJ_MTCE / -0.55)) %>%
      # Aggregate by fuel compute each province's percentage, by fuel
      group_by(fuel, year) %>%
      mutate(pct = value / sum(value)) %>%
      ungroup %>%
      mutate(fuel = replace(fuel, fuel == "solar", "solar PV")) %>%
      select(-value) %>%
      replace_na(list(pct = 0)) ->
      L123.pct_province_elec_F

    # Electricity generation inputs by fuel and province
    # Note that fuel inputs are only available for selected fuels; province_share_data uses this relevant subset
    L123.pct_province_elec_F %>%
      filter(fuel %in% unique(L123.in_EJ_R_elec_F_Yh$fuel), year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(L123.in_EJ_R_elec_F_Yh %>%
                                 filter(GCAM_region_ID == gcamchina.REGION_ID), by = c("fuel", "year")) %>%
      mutate(value = value * pct) %>%
      select(-pct, -GCAM_region_ID) ->
      L123.in_EJ_province_elec_F

    # Electricity generation outputs by fuel and province
    L123.pct_province_elec_F %>%
      filter(fuel %in% unique(L123.out_EJ_R_elec_F_Yh$fuel), year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(L123.out_EJ_R_elec_F_Yh %>%
                                 filter(GCAM_region_ID == gcamchina.REGION_ID), by = c("fuel", "year")) %>%
      mutate(value = value * pct) %>%
      select(-pct, -GCAM_region_ID) ->
      L123.out_EJ_province_elec_F

    # ELECTRICITY - OWNUSE
    # Electricity own use by province
    L126.in_EJ_R_elecownuse_F_Yh %>%
      filter(GCAM_region_ID == gcamchina.REGION_ID) %>%
      left_join_error_no_match(L126.out_EJ_R_elecownuse_F_Yh, by = c("GCAM_region_ID", "sector", "fuel", "year")) %>%
      mutate(value = value.x - value.y) %>%
      select(-value.x, -value.y) ->
      L123.net_EJ_CHINA_ownuse

    # Then build table with each province's share of the national ownuse. Note that this is assumed invariant over time.
    # Net own use = national total by each province's share
    L123.out_EJ_province_elec_F %>%
      group_by(province, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      group_by(year) %>%
      mutate(value = value / sum(value)) %>%
      ungroup  %>%
      mutate(sector = "electricity ownuse", fuel = "electricity") %>%
      replace_na(list(value = 0)) %>%
      left_join_error_no_match(L123.net_EJ_CHINA_ownuse, by = c("sector", "fuel", "year")) %>%
      mutate(value = value.x * value.y) %>%
      select(province, sector, fuel, year, value) ->
      L123.net_EJ_province_ownuse_elec

    # The input of the electricity_net_ownuse sector is equal to sum of all generation (industrial CHP + electric sector)
    L123.out_EJ_province_elec_F %>%
      bind_rows(L132.out_EJ_province_indchp_F) %>%
      group_by(province, year) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      mutate(sector = "electricity ownuse", fuel = "electricity") ->
      L123.in_EJ_province_ownuse_elec

    # Output of electricity_net_ownuse sector is equal to input minus ownuse "net" energy
    L123.in_EJ_province_ownuse_elec %>%
      left_join_error_no_match(L123.net_EJ_province_ownuse_elec, by = c("province", "sector", "fuel", "year")) %>%
      mutate(value = value.x - value.y) %>%
      select(province, sector, fuel, year, value) ->
      L123.out_EJ_province_ownuse_elec


    # ===================================================
    # 3.Produce outputs
    L123.in_EJ_province_elec_F %>%
      add_title("Electricity sector energy consumption by province and fuel") %>%
      add_units("EJ") %>%
      add_comments("The energy consumption is generated by dividing data on capacity factors by province by national average capacity factor") %>%
      add_legacy_name("L123.in_EJ_province_elec_F") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/CPSY_GWh_province_F_elec_out",
                     "L123.in_EJ_R_elec_F_Yh",
                     "L101.inNBS_Mtce_province_S_F") ->
      L123.in_EJ_province_elec_F

    L123.out_EJ_province_elec_F %>%
      add_title("Electricity generation by province and fuel") %>%
      add_units("EJ") %>%
      add_comments("The electricity generation is generated by dividing data on generation by province by fuel") %>%
      add_legacy_name("L123.out_EJ_province_elec_F") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/CPSY_GWh_province_F_elec_out",
                     "L123.out_EJ_R_elec_F_Yh",
                     "L101.inNBS_Mtce_province_S_F") ->
      L123.out_EJ_province_elec_F

    L123.in_EJ_province_ownuse_elec %>%
      add_title("Input to electricity net ownuse by province") %>%
      add_units("EJ") %>%
      add_comments("The electricity net ownuse is generated by dividing data on generation by province by fuel") %>%
      add_legacy_name("L123.in_EJ_province_ownuse_elec") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/CPSY_GWh_province_F_elec_out",
                     "L123.out_EJ_R_elec_F_Yh",
                     "L101.inNBS_Mtce_province_S_F",
                     "L132.out_EJ_province_indchp_F") ->
      L123.in_EJ_province_ownuse_elec

    L123.out_EJ_province_ownuse_elec %>%
      add_title("Output of electricity net ownuse by province") %>%
      add_units("EJ") %>%
      add_comments("The electricity net ownuse is generated by dividing data on generation by province by fuel") %>%
      add_legacy_name("L123.out_EJ_province_ownuse_elec") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/CPSY_GWh_province_F_elec_out",
                     "L123.out_EJ_R_elec_F_Yh",
                     "L126.in_EJ_R_elecownuse_F_Yh",
                     "L126.out_EJ_R_elecownuse_F_Yh",
                     "L101.inNBS_Mtce_province_S_F",
                     "L132.out_EJ_province_indchp_F") ->
      L123.out_EJ_province_ownuse_elec

    return_data(L123.in_EJ_province_elec_F, L123.out_EJ_province_elec_F, L123.in_EJ_province_ownuse_elec, L123.out_EJ_province_ownuse_elec)
  } else {
    stop("Unknown command")
  }
}
