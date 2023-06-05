# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcam.china_LA144.Building_CHINA
#'
#' Downscaling each province and sector's shares of  building energy use by fuel
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L144.in_EJ_province_bld_F_U} \code{L144.flsp_bm2_province_bld} The corresponding file in the
#' original data system was \code{LA142.Building.R} (gcam-china level1).
#' @details Buildings sector energy consumption
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter group_by mutate select summarise
#' @importFrom tidyr gather spread
#' @author BY January 2020

module_gcam.china_LA144.Building_CHINA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L142.in_EJ_R_bld_F_Yh",
             "L144.flsp_bm2_R_comm_Yh",
             FILE="gcam-china/province_names_mappings",
             FILE="gcam-china/cR_BldS_F_U_share",
             FILE="gcam-china/floorspace_m2_province_Yh",
             FILE="gcam-china/urban_pop_share_province_Yh",
             "L101.Pop_thous_province",
             "L101.inNBS_Mtce_province_S_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L144.in_EJ_province_bld_F_U",
             "L144.flsp_bm2_province_bld"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L142.in_EJ_R_bld_F_Yh <- get_data(all_data, "L142.in_EJ_R_bld_F_Yh")
    L144.flsp_bm2_R_comm_Yh <- get_data(all_data, "L144.flsp_bm2_R_comm_Yh" )
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings")
    cR_BldS_F_U_share <- get_data(all_data, "gcam-china/cR_BldS_F_U_share")
    flsp_m2pc_province_Yh <- get_data(all_data, "gcam-china/floorspace_m2_province_Yh")
    urban_pop_share_province_Yh <- get_data(all_data, "gcam-china/urban_pop_share_province_Yh")
    L101.Pop_thous_province <- get_data(all_data, "L101.Pop_thous_province")
    L101.inNBS_Mtce_province_S_F <- get_data(all_data, "L101.inNBS_Mtce_province_S_F")


    # Silence package checks
    GCAM_region_ID <- Rural <- Urban <- Year <- climate.region <- fuel <- pop <- pop.total <-
      pop.urban <- province <- sector <- sector.match <- service <- share <- value <- value.CHINA <-
      value.fuel <- value.province <- value.total <- value.x <- value.y <- year <- NULL

    # ===================================================

    # Perform computations
    # Subset residential and commercial from the energy balance table, and only the fuels that are part of the GCAM buildings sector. Sort by fuel.
    # Calculating each province and sector's (res/comm) shares of CHINA building energy use, by fuel
    # NOTE: Using NBS rather than IEA for nation-level disaggregation between residential and commercial
    L144.in_Mtce_province_bld_F_unscaled <- L101.inNBS_Mtce_province_S_F %>%
      filter(sector %in% c("comm", "resid_urban", "resid_rural"), fuel %in% L142.in_EJ_R_bld_F_Yh$fuel)

    # figure out which historical years have zeros across all fuels/sectors, we will extrapolate this data
    # first, group by each province and year, sum across all fuels/sectors
    L144.in_Mtce_province_bld_F_unscaled_province_year_totals <- L144.in_Mtce_province_bld_F_unscaled %>%
      group_by(province, year) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup()

    L144.in_Mtce_province_bld_F_unscaled <- L144.in_Mtce_province_bld_F_unscaled %>%
      # must use left join as there are NAs in the original dataframe
      left_join(L144.in_Mtce_province_bld_F_unscaled_province_year_totals, by = c("province", "year"), suffix = c(".fuel", ".total")) %>%
      # zero out NAs in years where some values are NA but not all %>%
      mutate(value.fuel = if_else((is.na(value.fuel) & value.total != 0), 0, value.fuel)) %>%
      select(-value.total) %>%
      rename(value = value.fuel) %>%
      # use gcam_interp rule = 2 to fill out data in years where the entire province is NA
      arrange(province, sector, fuel, year) %>%
      group_by(province, sector, fuel) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup()

    # need to add some mapping for traditional biomass since that was not explicit in the energy balance
    L144.in_Mtce_province_bld_F_unscaled.bio <- L144.in_Mtce_province_bld_F_unscaled %>%
      filter(fuel == "coal") %>%
      mutate(fuel = if_else(sector == "resid_rural", "traditional biomass", "biomass"))

    # bind back together with the rest of the fuels
    L144.in_Mtce_province_bld_F_unscaled <- bind_rows( L144.in_Mtce_province_bld_F_unscaled, L144.in_Mtce_province_bld_F_unscaled.bio )

    # NOTE: the current service partitioning does not allow heat in resid_rural so just zero it out
    L144.in_Mtce_province_bld_F_unscaled <- L144.in_Mtce_province_bld_F_unscaled %>%
      mutate(value = if_else((sector == "resid_rural" & fuel == "heat"), 0, value))

    # Aggregate by fuel to calculate each province and sector's portional allocation
    L144.in_Mtce_CHINA_bld_F_unscaled <- L144.in_Mtce_province_bld_F_unscaled %>%
      group_by(year, fuel) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # Calculate the portional allocation of total China buildings energy use (by each fuel) to each province and sector
    L144.in_pct_province_bld_F <- L144.in_Mtce_province_bld_F_unscaled %>%
      left_join_error_no_match(L144.in_Mtce_CHINA_bld_F_unscaled, by = c("year", "fuel"), suffix = c(".province", ".CHINA")) %>%
      mutate(value = value.province/value.CHINA) %>%
      select(-value.province, -value.CHINA)

    # Apportion nation-level energy by fuel to provinces and sectors
    # Now aggregate the building sector energy consumption to entire GCAM regions.
    L142.in_EJ_R_bld_F_Yh %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      L144.in_EJ_R_bldtot_F_Yh

    # replace NaN with "0" in value column
    L144.in_EJ_R_bldtot_F_Yh$value[is.na(L144.in_EJ_R_bldtot_F_Yh$value)] <- 0

    # select the aggregate building sector energy consumption for China
    L144.in_EJ_R_bldtot_F_Yh %>%
      filter(GCAM_region_ID == gcamchina.REGION_ID) %>%
      ungroup %>%
      select(-GCAM_region_ID) ->
      L144.in_EJ_R_bldtot_F_Yh_CHINA

    # Apportion nation-level energy by fuel to provinces and sectors by scaling by the portion of
    # total China building energy use by fuel for each province and sector from the SEDS table.
    L144.in_EJ_province_bld_F <- L144.in_pct_province_bld_F %>%
      left_join_error_no_match(L144.in_EJ_R_bldtot_F_Yh_CHINA, by = c("year", "fuel")) %>%
      mutate(value = value.x * value.y) %>%
      select(province, sector, fuel, year, value)

    #Apportion fuel by services.
    L144.cR_S_F_U_share <- cR_BldS_F_U_share %>%
      gather(c("Heating", "Cooling", "Lighting", "WH & C", "Appliance"), key = "service", value = "share")

    L144.in_EJ_province_bld_F_U <- L144.cR_S_F_U_share %>%
      # must use left join, as number of rows is changing
      left_join(province_names_mappings, by = c("climate.region")) %>%
      select(climate.region, sector, fuel, service, share, province) %>%
      # must use left join, as number of rows is changing
      left_join(L144.in_EJ_province_bld_F, by = c("province", "sector", "fuel")) %>%
      mutate(value = value * share) %>%
      # replace NA with 0
      mutate(value = if_else(is.na(value), 0, value)) %>%
      select(province, sector, fuel, service, year, value) %>%
      na.omit()

    # Calculate historical commercial floorspace
    # TODO: better basis for commercial floorspace
    L144.flsp_bm2_CHINA_comm <- L144.flsp_bm2_R_comm_Yh %>%
      filter(GCAM_region_ID == gcamchina.REGION_ID) %>%
      mutate(sector = "comm")

    L144.in_EJ_province_bld <- L144.in_EJ_province_bld_F_U %>%
      group_by(province, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    L144.in_EJ_province_bld_comm <- L144.in_EJ_province_bld %>%
      filter(sector == "comm")

    L144.in_EJ_CHINA_bld_comm <- L144.in_EJ_province_bld_comm %>%
      group_by(sector, year) %>%
      summarise(value = sum(value))

    L144.in_pct_province_bld_comm <- L144.in_EJ_province_bld_comm %>%
      left_join_error_no_match(L144.in_EJ_CHINA_bld_comm, by = c("sector", "year"), suffix = c(".province", ".total")) %>%
      mutate(value = value.province/value.total,
             value = if_else(is.na(value), 0, value)) %>%
      select(-value.province, - value.total)

    L144.flsp_bm2_province_bld_comm <- L144.in_pct_province_bld_comm %>%
      left_join_error_no_match(L144.flsp_bm2_CHINA_comm, by = c("year", "sector")) %>%
      mutate(value = value.x * value.y,
             sector.match = "comm") %>%
      select(province, sector, sector.match, year, value)

    # Residential floorspace: first compute absolute floorspace by multiplying per capita data by population
    # Compute urban population in historical years
    total_pop_thous_prov_Yh <- subset(L101.Pop_thous_province, year %in% HISTORICAL_YEARS)

    urban_pop_share_province_Yh.long <- urban_pop_share_province_Yh %>%
      gather(key = "year", value = "value", as.character(HISTORICAL_YEARS)) %>%
      mutate(year = as.numeric(year)) %>%
      left_join_error_no_match(province_names_mappings, by = c("province.name")) %>%
      select(province, year, value)

    urban_pop_thous_prov_Yh <- total_pop_thous_prov_Yh %>%
      left_join_error_no_match(urban_pop_share_province_Yh.long, by = c("province", "year")) %>%
      mutate(pop = pop * value)

    # Compute rural population in historical years
    rural_pop_thous_prov_Yh <- total_pop_thous_prov_Yh %>%
      left_join_error_no_match(urban_pop_thous_prov_Yh, by = c("province", "year"), suffix = c(".total", ".urban")) %>%
      mutate(pop = pop.total - pop.urban) %>%
      select(-pop.total, -pop.urban)

    # Compute urban and rural residential floorspace in the provinces by multiplying per capita data by population
    flsp_m2pc_province_Yh_urban_rural <- flsp_m2pc_province_Yh %>%
      left_join_error_no_match(province_names_mappings, by = c("province.name")) %>%
      filter(Year %in% unique(total_pop_thous_prov_Yh$year)) %>%
      rename(year = Year) %>%
      left_join_error_no_match(urban_pop_thous_prov_Yh, by = c("province", "year")) %>%
      mutate(Urban = Urban * pop * 1000) %>%
      select(-pop, -value) %>%
      left_join_error_no_match(rural_pop_thous_prov_Yh, by = c("province", "year")) %>%
      mutate(Rural = Rural * pop * 1000)  %>%
      select(-pop, -value)

    flsp_bm2_province_Yh <- flsp_m2pc_province_Yh_urban_rural %>%
      # TODO: convert from m^2 to billion-m^2
      mutate(Urban = Urban / CONV_ONES_BIL,
             Rural = Rural / CONV_ONES_BIL)

    # Combine commercial and residential into one table
    L144.flsp_bm2_prov_res_urban <- flsp_bm2_province_Yh %>%
      select(province, year, Urban) %>%
      rename(value = Urban) %>%
      mutate(sector = "resid_urban")

    L144.flsp_bm2_prov_res_rural <- flsp_bm2_province_Yh %>%
      select(province, year, Rural) %>%
      rename(value = Rural) %>%
      mutate(sector = "resid_rural")

    # Copy 1975 values for 1971-1974
    L144.flsp_bm2_prov_res_urban_1971_1974 <- L144.flsp_bm2_prov_res_urban %>%
      filter(year == min(MODEL_BASE_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
      filter(year < min(MODEL_BASE_YEARS))

    L144.flsp_bm2_prov_res_rural_1971_1974 <- L144.flsp_bm2_prov_res_urban %>%
      filter(year == min(MODEL_BASE_YEARS)) %>%
      select(-year) %>%
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
      filter(year < min(MODEL_BASE_YEARS))

    L144.flsp_bm2_province_bld_resid <- bind_rows(L144.flsp_bm2_prov_res_urban_1971_1974,
                                            L144.flsp_bm2_prov_res_urban,
                                            L144.flsp_bm2_prov_res_rural_1971_1974,
                                            L144.flsp_bm2_prov_res_rural) %>%
      mutate(sector.match = "resid")

    L144.flsp_bm2_province_bld <- bind_rows(L144.flsp_bm2_province_bld_comm,
                                            L144.flsp_bm2_province_bld_resid)

    # ===================================================
    # Write outputs

    L144.in_EJ_province_bld_F_U %>%
      add_title("Buildings energy consumption by province, sector (res/comm),fuel, and service") %>%
      add_units("EJ") %>%
      add_comments("NOTE: Using NBS rather than IEA for nation-level disaggregation between residential and commercial") %>%
      add_legacy_name("L142.in_EJ_province_bld_F_U") %>%
      add_precursors("L142.in_EJ_R_bld_F_Yh",
                     FILE="gcam-china/province_names_mappings",
                     FILE="gcam-china/cR_BldS_F_U_share",
                     "L101.inNBS_Mtce_province_S_F")  ->
      L144.in_EJ_province_bld_F_U

    L144.flsp_bm2_province_bld %>%
      add_title("Buildings floorspace by province and sector (res/comm)") %>%
      add_units("billion-m^2") %>%
      add_comments("Comm floorspace calculated on basis of energy consumption by province, Resid floorspace calculated with urban/rural population data and flsp proj") %>%
      add_legacy_name("L142.flsp_bm2_province_bld") %>%
      add_precursors("L142.in_EJ_R_bld_F_Yh",
                     "L144.flsp_bm2_R_comm_Yh",
                     FILE="gcam-china/province_names_mappings",
                     FILE="gcam-china/cR_BldS_F_U_share",
                     FILE="gcam-china/floorspace_m2_province_Yh",
                     FILE="gcam-china/urban_pop_share_province_Yh",
                     "L101.Pop_thous_province",
                     "L101.inNBS_Mtce_province_S_F")  ->
      L144.flsp_bm2_province_bld

    return_data(L144.in_EJ_province_bld_F_U,
                L144.flsp_bm2_province_bld)
  }
  else {
    stop("Unknown command")
  }
}
