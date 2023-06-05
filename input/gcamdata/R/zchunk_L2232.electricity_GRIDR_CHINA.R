# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L2232.electricity_GRIDR_CHINA
#'
#' Generate GCAM-CHINA model inputs for electrcity trade sectors at the level of grid regions.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2232.DeleteSupplysector_CHINAelec}, \code{L2232.Supplysector_CHINAelec},
#' \code{L2232.SubsectorShrwtFllt_CHINAelec}, \code{L2232.SubsectorInterp_CHINAelec}, \code{L2232.SubsectorLogit_CHINAelec},
#' \code{L2232.TechShrwt_CHINAelec}, \code{L2232.TechCoef_CHINAelec}, \code{L2232.Production_exports_CHINAelec},
#' \code{L2232.Supplysector_elec_GRIDR}, \code{L2232.ElecReserve_GRIDR}, \code{L2232.SubsectorShrwtFllt_elec_GRIDR},
#' \code{L2232.SubsectorInterp_elec_GRIDR}, \code{L2232.SubsectorLogit_elec_GRIDR}, \code{L2232.TechShrwt_elec_GRIDR},
#' \code{L2232.TechCoef_elec_GRIDR}, \code{L2232.TechCoef_elecownuse_GRIDR}, \code{L2232.Production_imports_GRIDR},
#' \code{L2232.Production_elec_gen_GRIDR}, \code{L2232.StubTechElecMarket_backup_CHINA}. The corresponding file in the
#' original data system was \code{L2232.electricity_GRIDR_CHINA.R} (gcam-china level2).
#' @details This code file only builds the electric sector model input if the demand is being resolved at the level of the grid regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr gather spread
#' @author YangLiu Jan 2020
module_gcamchina_L2232.electricity_GRIDR_CHINA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "energy/A23.sector",
             FILE = "gcam-china/A232.structure",
             "L123.in_EJ_province_ownuse_elec",
             "L123.out_EJ_province_ownuse_elec",
             "L122.in_EJ_province_refining_F",
             "L123.out_EJ_province_elec_F",
             "L132.in_EJ_province_indchp_F",
             "L132.in_EJ_province_indfeed_F",
             "L132.in_EJ_province_indnochp_F",
             "L126.IO_R_electd_F_Yh",
             "L1321.in_EJ_province_cement_F_Y",
             "L1322.in_EJ_province_Fert_Yh",
             "L144.in_EJ_province_bld_F_U",
             "L154.in_EJ_province_trn_F",
             "L132.out_EJ_province_indchp_F",
             "L1232.out_EJ_sR_elec_CHINA",
             "L223.StubTechMarket_backup_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2232.DeleteSupplysector_CHINAelec",
             "L2232.Supplysector_CHINAelec",
             "L2232.SubsectorShrwtFllt_CHINAelec",
             "L2232.SubsectorInterp_CHINAelec",
             "L2232.SubsectorLogit_CHINAelec",
             "L2232.TechShrwt_CHINAelec",
             "L2232.TechCoef_CHINAelec",
             "L2232.Production_exports_CHINAelec",
             "L2232.Supplysector_elec_GRIDR",
             "L2232.ElecReserve_GRIDR",
             "L2232.SubsectorShrwtFllt_elec_GRIDR",
             "L2232.SubsectorInterp_elec_GRIDR",
             "L2232.SubsectorLogit_elec_GRIDR",
             "L2232.TechShrwt_elec_GRIDR",
             "L2232.TechCoef_elec_GRIDR",
             "L2232.TechCoef_elecownuse_GRIDR",
             "L2232.Production_imports_GRIDR",
             "L2232.Production_elec_gen_GRIDR",
             "L2232.StubTechElecMarket_backup_CHINA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    grid_region <- province <- value <- year <- region <- supplysector <-
      calOutputValue <- coefficient <- cogeneration <- consumption <-
      generation <- imports <- in_ownuse <- market.name <- minicam.energy.input <-
      exports <- net.exports <- net.supply <- net_ownuse <- ownuse <- ownuse_coef <-
      subsector <- subsector.logit <- subsector.logit.type <- technology <-
      technology.logit <- technology.logit.type <- fuel <- sector <- GCAM_region_ID <-
      value.x <- value.y <- sector.x <- grid.region <- NULL  # silence package check notes

    # Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = T)
    A23.sector <- get_data(all_data, "energy/A23.sector", strip_attributes = T)
    A232.structure <- get_data(all_data, "gcam-china/A232.structure", strip_attributes = T)
    L123.in_EJ_province_ownuse_elec <- get_data(all_data, "L123.in_EJ_province_ownuse_elec", strip_attributes = T)
    L123.out_EJ_province_ownuse_elec <- get_data(all_data, "L123.out_EJ_province_ownuse_elec", strip_attributes = T)
    L132.out_EJ_province_indchp_F <- get_data(all_data, "L132.out_EJ_province_indchp_F", strip_attributes = T)
    L1232.out_EJ_sR_elec_CHINA <- get_data(all_data, "L1232.out_EJ_sR_elec_CHINA", strip_attributes = T)
    L223.StubTechMarket_backup_CHINA <- get_data(all_data, "L223.StubTechMarket_backup_CHINA", strip_attributes = T)
    L126.IO_R_electd_F_Yh <- get_data(all_data, "L126.IO_R_electd_F_Yh", strip_attributes = T)
    L122.in_EJ_province_refining_F <- get_data(all_data, "L122.in_EJ_province_refining_F", strip_attributes = T)
    L123.out_EJ_province_elec_F <- get_data(all_data, "L123.out_EJ_province_elec_F", strip_attributes = T)
    L132.in_EJ_province_indchp_F <- get_data(all_data, "L132.in_EJ_province_indchp_F", strip_attributes = T)
    L132.in_EJ_province_indfeed_F <- get_data(all_data, "L132.in_EJ_province_indfeed_F", strip_attributes = T)
    L132.in_EJ_province_indnochp_F <- get_data(all_data, "L132.in_EJ_province_indnochp_F", strip_attributes = T)
    L1321.in_EJ_province_cement_F_Y <- get_data(all_data, "L1321.in_EJ_province_cement_F_Y", strip_attributes = T)
    L1322.in_EJ_province_Fert_Yh <- get_data(all_data, "L1322.in_EJ_province_Fert_Yh", strip_attributes = T)
    L144.in_EJ_province_bld_F_U <- get_data(all_data, "L144.in_EJ_province_bld_F_U", strip_attributes = T)
    L154.in_EJ_province_trn_F <- get_data(all_data, "L154.in_EJ_province_trn_F", strip_attributes = T)

    # calculate  L126.in_EJ_province_td_elec
    L126.in_EJ_province_S_F <- bind_rows(L122.in_EJ_province_refining_F, L123.out_EJ_province_elec_F,
                                         L132.in_EJ_province_indchp_F, L132.in_EJ_province_indfeed_F,
                                         L132.in_EJ_province_indnochp_F, L1321.in_EJ_province_cement_F_Y,
                                         L1322.in_EJ_province_Fert_Yh, L144.in_EJ_province_bld_F_U,
                                         L154.in_EJ_province_trn_F)

    # Final energy by fuel
    L126.in_EJ_province_F <- L126.in_EJ_province_S_F %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(province, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # ELECTRICITY TRANSMISSION AND DISTRIBUTION
    # Compile each province's total elec consumption: refining, bld, ind, trn.
    L126.in_EJ_province_elec <- L126.in_EJ_province_F %>%
      filter(fuel == "electricity")

    # Deriving electricity T&D output as the sum of all tracked demands of electricity
    L126.out_EJ_province_td_elec <- L126.in_EJ_province_elec %>%
      mutate(sector = "elect_td") %>%
      select(province, sector, fuel, year, value)

    # Assigning all provinces the national average T&D coefficients from L126.IO_R_electd_F_Yh
    L126.in_EJ_province_td_elec <- L126.out_EJ_province_td_elec %>%
      left_join(L126.IO_R_electd_F_Yh %>% filter(GCAM_region_ID == gcamchina.REGION_ID), by = c("fuel", "year")) %>%
      # province input elec = province output elec * coefficient
      mutate(value = value.x * value.y) %>%
      select(province, sector = sector.x, fuel, year, value)



    # This chunk builds the electric sector model with demand resolved at the grid region level.

    # A vector of CHINA grid region names
    province_names_mappings %>% rename(grid_region = grid.region) -> province_names_mappings
    grid_regions <- province_names_mappings$grid_region %>%
      unique %>%
      sort

    # PART 1: THE CHINA REGION
    # L2232.DeleteSupplysector_CHINAelec: Remove the electricity sectors of the CHINA region (incl. net_ownuse)
    # Remove the CHINA electricity sector, and replace with electricity trade
    tibble(region = gcamchina.REGION,
           supplysector = c("electricity", "electricity_net_ownuse")) ->
      L2232.DeleteSupplysector_CHINAelec

    # L2232.Supplysector_CHINAelec: supplysector for electricity trade sector in the CHINA region,
    # including logit exponent between grid regions
    # All of the supplysector information is the same as before, except the logit exponent
    A232.structure %>%
      filter(region == gcamchina.REGION) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = subsector.logit,
             logit.type = subsector.logit.type) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME)) ->
      L2232.Supplysector_CHINAelec

    # L2232.SubsectorShrwtFllt_CHINAelec: subsector (grid region) share-weights in CHINA electricity trade
    # No need to read in subsector logit exponents, which are applied to the technology competition
    A232.structure %>%
      filter(region == gcamchina.REGION) %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      repeat_add_columns(tibble(grid_region = grid_regions)) %>%
      mutate(subsector = replace(subsector, grepl("grid_region", subsector),
                                 paste(grid_region[grepl("grid_region", subsector)], "electricity trade", sep = " ")),
             year.fillout = min(MODEL_BASE_YEARS),
             share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) ->
      L2232.SubsectorShrwtFllt_CHINAelec

    # L2232.SubsectorInterp_CHINAelec: temporal interpolation of subsector share-weights in CHINA electricity trade
    # NOTE: this just carries the base year share-weights forward;
    # regions that don't export in the base year don't export at all
    L2232.SubsectorShrwtFllt_CHINAelec %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") ->
      L2232.SubsectorInterp_CHINAelec

    # L2232.SubsectorLogit_CHINAelec: logit exponent of subsector in CHINA electricity trade
    # NOTE: There is only one tech per subsector in the GRIDR markets so the logit choice does not matter
    L2232.SubsectorShrwtFllt_CHINAelec %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      left_join(select(A232.structure, region,
                       logit.exponent = technology.logit,
                       logit.type = technology.logit.type),
                by = "region") %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)) ->
      L2232.SubsectorLogit_CHINAelec

    # L2232.TechShrwt_CHINAelec: technology share-weights in CHINA electricity trade
    A232.structure %>%
      filter(region == gcamchina.REGION) %>%
      select(LEVEL2_DATA_NAMES[["Tech"]]) %>%
      repeat_add_columns(tibble(grid_region = grid_regions)) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(subsector = replace(subsector, grepl("grid_region", subsector),
                                 paste(grid_region[grepl("grid_region", subsector)], "electricity trade", sep = " ")),
             technology = replace(technology, grepl("grid_region", technology),
                                  paste(grid_region[grepl("grid_region", technology)], "electricity trade", sep = " ")),
             share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]], "share.weight", "grid_region") ->
      L2232.TechShrwt_CHINAelec

    # L2232.TechCoef_CHINAelec: technology coefficients and market names in CHINA electricity trade
    L2232.TechShrwt_CHINAelec %>%
      left_join_error_no_match(select(A232.structure, region, minicam.energy.input), by = "region") %>%
      mutate(coefficient = 1, market.name = grid_region) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L2232.TechCoef_CHINAelec

    # Compile flows of electricity in each GRIDR region:
    # generation, cogeneration, ownuse, and consumption by all sectors
    # to calculate exports, imports, and net supply

    # Generation by grid region
    L1232.out_EJ_sR_elec_CHINA %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(grid_region = grid.region) %>%
      select(grid_region, year, generation = value) ->
      L2232.out_EJ_sR_elec

    # Cogeneration is not included in the grid region totals; need to add it here for balance
    L132.out_EJ_province_indchp_F %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(select(province_names_mappings, province, grid_region), by = "province") %>%
      group_by(grid_region, year) %>%
      summarise(cogeneration = sum(value)) %>%
      ungroup ->
      L2232.out_EJ_sR_indchp_F

    # Calculate net own use in each grid region
    L123.in_EJ_province_ownuse_elec %>%
      rename(in_ownuse = value) %>%
      left_join_error_no_match(L123.out_EJ_province_ownuse_elec, by = c("province", "sector", "fuel", "year")) %>%
      # Net own use is calculated as total generation minus net outputs
      mutate(net_ownuse = in_ownuse - value) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(select(province_names_mappings, province, grid_region), by = "province") %>%
      group_by(grid_region, year) %>%
      summarise(ownuse = sum(net_ownuse)) %>%
      ungroup ->
      L2232.net_EJ_sR_ownuse_elec

    # Comsumption: the sum of all demands in each GRIDR region, equal to the input to the elect_td sectors
    L126.in_EJ_province_td_elec %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(select(province_names_mappings, province, grid_region), by = "province") %>%
      group_by(grid_region, year) %>%
      summarise(consumption = sum(value)) %>%
      ungroup ->
      L2232.in_EJ_sR_td_elec

    # Complie all flows and calculate exports, imports and net supply
    L2232.TechShrwt_CHINAelec %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L2232.out_EJ_sR_elec, by = c("grid_region", "year")) %>%
      left_join_error_no_match(L2232.out_EJ_sR_indchp_F, by = c("grid_region", "year")) %>%
      left_join_error_no_match(L2232.net_EJ_sR_ownuse_elec, by = c("grid_region", "year")) %>%
      left_join_error_no_match(L2232.in_EJ_sR_td_elec, by = c("grid_region", "year")) %>%
      # Calculate net exports: generation + cogeneration - ownuse - consumption
      mutate(net.exports = generation + cogeneration - ownuse - consumption,
             # Split net exports into gross imports and exports:
             # When net exports are positive, exports equal net exports, and imports are zero;
             # When net exports are negative, imports equal minus net exports, and exports are zero
             imports = pmax(0, -1 * net.exports),
             exports = pmax(0, net.exports),
             # Calculate consumption from domestic sources: total consumption minus gross imports
             net.supply = consumption - imports) ->
      L2232.elec_flows_GRIDR

    # L2232.Production_exports_CHINAelec: calibrated exports of electricity from grid regions to shared CHINA region
    L2232.elec_flows_GRIDR %>%
      mutate(calOutputValue = round(exports, digits = energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L2232.Production_exports_CHINAelec


    # PART 2: THE GRIDR REGIONS
    # Some of the information read in about these regions is in the primary electricity_CHINA code file

    # Create the GRIDR region structure tibble
    A232.structure %>%
      filter(region == "grid_region") %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = grid_regions)) %>%
      mutate(market.name = replace(market.name, grepl("grid_region", market.name),
                                   region[grepl("grid_region", market.name)])) ->
      A232.GRIDRstructure

    # L2232.Supplysector_elec_GRIDR: supplysector information for electricity passthrough sectors in the GRIDR regions
    A232.GRIDRstructure %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = subsector.logit,
             logit.type = subsector.logit.type) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME)) ->
      L2232.Supplysector_elec_GRIDR

    # L2232.ElecReserve_GRIDR: electricity reserve margin and avg grid capacity factor in the grid regions
    A23.sector %>%
      filter(supplysector == "electricity") %>%
      repeat_add_columns(tibble(region = grid_regions)) %>%
      select(LEVEL2_DATA_NAMES[["ElecReserve"]]) ->
      L2232.ElecReserve_GRIDR

    # L2232.SubsectorShrwtFllt_elec_GRIDR: subsector (provinces) share-weights
    # for electricity passthrough sectors in grid regions
    A232.GRIDRstructure %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS), share.weight = 1) ->
      L2232.SubsectorShrwtFllt_elec_GRIDR

    # L2232.SubsectorInterp_elec_GRIDR: temporal interpolation of subsector (provinces) share-weights
    # for electricity passthrough sectors in grid regions
    L2232.SubsectorShrwtFllt_elec_GRIDR %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") ->
      L2232.SubsectorInterp_elec_GRIDR

    # L2232.SubsectorShrwtFllt_elec_GRIDR: logit exponent of subsector (provinces) in grid regions
    # NOTE: There is only one tech per subsector in the GRIDR markets so the logit choice does not matter
    L2232.SubsectorShrwtFllt_elec_GRIDR %>%
      left_join(A232.GRIDRstructure %>%
                  select(region, technology.logit, technology.logit.type) %>%
                  unique, by = "region") %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = technology.logit,
             logit.type = technology.logit.type) %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)) ->
      L2232.SubsectorLogit_elec_GRIDR

    # L2232.TechShrwt_elec_GRIDR: technology share-weights in grid regions
    A232.GRIDRstructure %>%
      select(LEVEL2_DATA_NAMES[["Tech"]]) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1) ->
      L2232.TechShrwt_elec_GRIDR

    # L2232.TechCoef_elec_GRIDR: technology coefficients and market names for domestic supply in grid regions
    A232.GRIDRstructure %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # Own use coefficients will be done separately; delete from the table here
      filter(supplysector != "electricity_net_ownuse") %>%
      mutate(coefficient = 1) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L2232.TechCoef_elec_GRIDR

    # L2232.TechCoef_elecownuse_GRIDR: own use coefficients in the grid regions
    L2232.elec_flows_GRIDR %>%
      # Own use coefficients are total generation divided by total generation minus own use
      mutate(ownuse_coef = (generation + cogeneration) / (generation + cogeneration - ownuse)) ->
      L2232.elec_flows_GRIDR

    A232.GRIDRstructure %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      filter(supplysector == "electricity_net_ownuse") %>%
      left_join(select(L2232.elec_flows_GRIDR, grid_region, year, coefficient = ownuse_coef),
                by = c("region" = "grid_region", "year")) %>%
      group_by(region) %>%
      # Set future year own use coefficients the same as the base year coefficients
      mutate(coefficient = replace(coefficient, year %in% MODEL_FUTURE_YEARS, coefficient[year == max(MODEL_BASE_YEARS)])) %>%
      ungroup %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L2232.TechCoef_elecownuse_GRIDR

    # L2232.Production_imports_GRIDR: calibrated electricity imports (from CHINA region)
    L2232.TechCoef_elec_GRIDR %>%
      filter(year %in% MODEL_BASE_YEARS, market.name == gcamchina.REGION) %>%
      left_join_error_no_match(select(L2232.elec_flows_GRIDR, grid_region, year, imports),
                               by = c("region" = "grid_region", "year")) %>%
      mutate(calOutputValue = round(imports, digits = energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L2232.Production_imports_GRIDR

    # L2232.Production_elec_gen_GRIDR: calibrated net electricity generation (from within grid region)
    L2232.TechCoef_elec_GRIDR %>%
      filter(year %in% MODEL_BASE_YEARS, market.name != gcamchina.REGION) %>%
      left_join_error_no_match(select(L2232.elec_flows_GRIDR, grid_region, year, net.supply),
                               by = c("region" = "grid_region", "year")) %>%
      mutate(calOutputValue = round(net.supply, digits = energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L2232.Production_elec_gen_GRIDR

    # PART 3: THE provinceS
    # L2232.StubTechElecMarket_backup_CHINA_GRIDR: electric sector name for provinces
    # Reset the electric sector market to the grid regions (for backup calculations)
    L223.StubTechMarket_backup_CHINA %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]]) %>%
      left_join_error_no_match(select(province_names_mappings, electric.sector.market = grid_region, province),
                               by = c("region" = "province")) ->
      L2232.StubTechElecMarket_backup_CHINA


    # Produce outputs
    L2232.DeleteSupplysector_CHINAelec %>%
      add_title("Remove the electricity and net ownuse sectors of the CHINA region") %>%
      add_units("Uniteless") %>%
      add_comments("Remove the CHINA electricity supply sectors, and replace with electricity trade") %>%
      add_legacy_name("L2232.DeleteSupplysector_CHINAelec") ->
      L2232.DeleteSupplysector_CHINAelec

    L2232.Supplysector_CHINAelec %>%
      add_title("Supplysector for electricity sector in the CHINA region") %>%
      add_units("Uniteless") %>%
      add_comments("All of the supplysector information is the same as before") %>%
      add_comments("except including logit exponent between grid regions") %>%
      add_legacy_name("L2232.Supplysector_CHINAelec") %>%
      add_precursors("gcam-china/A232.structure") ->
      L2232.Supplysector_CHINAelec

    L2232.SubsectorShrwtFllt_CHINAelec %>%
      add_title("Subsector (grid region) share-weights in CHINA electricity trade") %>%
      add_units("Uniteless") %>%
      add_comments("No need to read in subsector logit exponents, which are applied to the technology competition") %>%
      add_legacy_name("L2232.SubsectorShrwtFllt_CHINAelec") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/A232.structure") ->
      L2232.SubsectorShrwtFllt_CHINAelec

    L2232.SubsectorInterp_CHINAelec %>%
      add_title("Table headers for temporal interpolation of subsector (grid region) share-weights in CHINA electricity trade") %>%
      add_units("Uniteless") %>%
      add_comments("This just carries the base year share-weights forward") %>%
      add_comments("Regions that don't export in the base year don't export at all") %>%
      add_legacy_name("L2232.SubsectorInterp_CHINAelec") %>%
      same_precursors_as("L2232.SubsectorShrwtFllt_CHINAelec") ->
      L2232.SubsectorInterp_CHINAelec

    L2232.SubsectorLogit_CHINAelec %>%
      add_title("Logit exponent of subsector (grid region) in CHINA electricity trade") %>%
      add_units("Uniteless") %>%
      add_comments("There is only one tech per subsector, so the logit choice does not matter") %>%
      add_legacy_name("L2232.SubsectorLogit_CHINAelec") %>%
      same_precursors_as("L2232.SubsectorShrwtFllt_CHINAelec") ->
      L2232.SubsectorLogit_CHINAelec

    L2232.TechShrwt_CHINAelec %>%
      add_title("Technology share-weights in the CHINA electricity trade") %>%
      add_units("Uniteless") %>%
      add_comments("Set the same value across all model years") %>%
      add_legacy_name("L2232.TechShrwt_CHINAelec") %>%
      add_precursors("gcam-china/A232.structure",
                     "gcam-china/province_names_mappings",
                     "L126.IO_R_electd_F_Yh") ->
      L2232.TechShrwt_CHINAelec

    L2232.TechCoef_CHINAelec %>%
      add_title("Technology coefficients and market names in the CHINA electricity trade") %>%
      add_units("Uniteless") %>%
      add_comments("Set the same value across all model years") %>%
      add_comments("Set grid region as market name") %>%
      add_legacy_name("L2232.TechCoef_CHINAelec") %>%
      same_precursors_as("L2232.TechShrwt_CHINAelec") ->
      L2232.TechCoef_CHINAelec

    L2232.Production_exports_CHINAelec %>%
      add_title("Calibrated exports of electricity from grid regions to shared CHINA region") %>%
      add_units("EJ") %>%
      add_comments("Electricity net exports are calulated as total generation minus consumption and own use") %>%
      add_comments("Cogeneration is included in total generation") %>%
      add_comments("When net exports are positive, exports equal net exports; when negative, exports are zero") %>%
      add_comments("Net own use is calculated as total generation minus net outputs") %>%
      add_comments("Consumption equals to the sum of input to the elect_td sectors") %>%
      add_legacy_name("L2232.Production_exports_CHINAelec") %>%
      same_precursors_as("L2232.TechShrwt_CHINAelec") %>%
      add_precursors("L123.in_EJ_province_ownuse_elec",
                     "L123.out_EJ_province_ownuse_elec",
                     "L122.in_EJ_province_refining_F",
                     "L123.out_EJ_province_elec_F",
                     "L132.in_EJ_province_indchp_F",
                     "L132.in_EJ_province_indfeed_F",
                     "L132.in_EJ_province_indnochp_F",
                     "L1321.in_EJ_province_cement_F_Y",
                     "L1322.in_EJ_province_Fert_Yh",
                     "L144.in_EJ_province_bld_F_U",
                     "L154.in_EJ_province_trn_F",
                     "L132.out_EJ_province_indchp_F",
                     "L1232.out_EJ_sR_elec_CHINA") ->
      L2232.Production_exports_CHINAelec

    L2232.Supplysector_elec_GRIDR %>%
      add_title("Supplysector information for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      add_legacy_name("L2232.Supplysector_elec_GRIDR") %>%
      add_precursors("gcam-china/A232.structure",
                     "gcam-china/province_names_mappings") ->
      L2232.Supplysector_elec_GRIDR

    L2232.ElecReserve_GRIDR %>%
      add_title("Electricity reserve margin and avg grid capacity factor in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("For electricity sector") %>%
      add_legacy_name("L2232.ElecReserve_GRIDR") %>%
      add_precursors("energy/A23.sector",
                     "gcam-china/province_names_mappings") ->
      L2232.ElecReserve_GRIDR

    L2232.SubsectorShrwtFllt_elec_GRIDR %>%
      add_title("Subsector (provinces) share-weights for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      add_legacy_name("L2232.SubsectorShrwtFllt_elec_GRIDR") %>%
      add_precursors("gcam-china/A232.structure",
                     "gcam-china/province_names_mappings") ->
      L2232.SubsectorShrwtFllt_elec_GRIDR

    L2232.SubsectorInterp_elec_GRIDR %>%
      add_title("Table header of temporal interpolation of subsector (provinces) share-weights for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      add_legacy_name("L2232.SubsectorInterp_elec_GRIDR") %>%
      same_precursors_as("L2232.SubsectorShrwtFllt_elec_GRIDR") ->
      L2232.SubsectorInterp_elec_GRIDR

    L2232.SubsectorLogit_elec_GRIDR %>%
      add_title("Logit exponent of subsector (provinces) for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      add_legacy_name("L2232.SubsectorLogit_elec_GRIDR") %>%
      same_precursors_as("L2232.SubsectorShrwtFllt_elec_GRIDR") ->
      L2232.SubsectorLogit_elec_GRIDR

    L2232.TechShrwt_elec_GRIDR %>%
      add_title("Technology share-weights for electricity passthrough sectors in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include electricity net own use and electricy domestic supply sectors") %>%
      add_legacy_name("L2232.TechShrwt_elec_GRIDR") %>%
      add_precursors("gcam-china/A232.structure",
                     "gcam-china/province_names_mappings") ->
      L2232.TechShrwt_elec_GRIDR

    L2232.TechCoef_elec_GRIDR %>%
      add_title("Technology coefficients and market names for electricity domestic supply in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Coefficients for electricity domestic supply") %>%
      add_legacy_name("L2232.TechCoef_elec_GRIDR") %>%
      same_precursors_as("L2232.TechShrwt_elec_GRIDR") ->
      L2232.TechCoef_elec_GRIDR

    L2232.TechCoef_elecownuse_GRIDR %>%
      add_title("Electricity own use coefficients in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Coefficients are calculated as total generation devided by total generation minus own use") %>%
      add_comments("Cogeneration is included in total generation") %>%
      add_comments("Set the coefficients for future years the same as in the model base year") %>%
      add_legacy_name("L2232.TechCoef_elecownuse_GRIDR") %>%
      same_precursors_as("L2232.TechShrwt_elec_GRIDR") %>%
      same_precursors_as("L2232.Production_exports_CHINAelec") ->
      L2232.TechCoef_elecownuse_GRIDR

    L2232.Production_imports_GRIDR %>%
      add_title("Calibrated electricity imports (from other grid regions)") %>%
      add_units("EJ") %>%
      add_comments("Imports equal mimus net exports when net exports are negative") %>%
      add_comments("Imports are zero when net exports are positive") %>%
      add_comments("Electricity net exports are calulated as total generation minus consumption and own use") %>%
      add_comments("Cogeneration is included in total generation") %>%
      add_comments("Net own use is calculated as total generation minus net outputs") %>%
      add_comments("Consumption equals to the sum of input to the elect_td sectors") %>%
      add_legacy_name("L2232.Production_imports_GRIDR") %>%
      same_precursors_as("L2232.TechCoef_elec_GRIDR") %>%
      same_precursors_as("L2232.Production_exports_CHINAelec") ->
      L2232.Production_imports_GRIDR

    L2232.Production_elec_gen_GRIDR %>%
      add_title("Calibrated net electricity generation (from within grid region)") %>%
      add_units("EJ") %>%
      add_comments("Net electricity generation is calculated as total consumption minus imports") %>%
      add_comments("Consumption equals to the sum of input to the elect_td sectors") %>%
      add_comments("Imports equal mimus net exports when net exports are negative") %>%
      add_comments("Imports are zero when net exports are positive") %>%
      add_comments("Electricity net exports are calulated as total generation minus consumption and own use") %>%
      add_comments("Cogeneration is included in total generation") %>%
      add_comments("Net own use is calculated as total generation minus net outputs") %>%
      add_legacy_name("L2232.Production_elec_gen_GRIDR") %>%
      same_precursors_as("L2232.TechCoef_elec_GRIDR") %>%
      same_precursors_as("L2232.Production_exports_CHINAelec") ->
      L2232.Production_elec_gen_GRIDR

    L2232.StubTechElecMarket_backup_CHINA %>%
      add_title("Electric sector name for provinces") %>%
      add_units("Unitless") %>%
      add_comments("Reset the electric sector market to the grid regions (for backup calculations)") %>%
      add_legacy_name("L2232.StubTechElecMarket_backup_CHINA") %>%
      add_precursors("L223.StubTechMarket_backup_CHINA") ->
      L2232.StubTechElecMarket_backup_CHINA

    return_data(L2232.DeleteSupplysector_CHINAelec, L2232.Supplysector_CHINAelec, L2232.SubsectorShrwtFllt_CHINAelec,
                L2232.SubsectorInterp_CHINAelec, L2232.SubsectorLogit_CHINAelec, L2232.TechShrwt_CHINAelec,
                L2232.TechCoef_CHINAelec, L2232.Production_exports_CHINAelec, L2232.Supplysector_elec_GRIDR,
                L2232.ElecReserve_GRIDR, L2232.SubsectorShrwtFllt_elec_GRIDR, L2232.SubsectorInterp_elec_GRIDR,
                L2232.SubsectorLogit_elec_GRIDR, L2232.TechShrwt_elec_GRIDR, L2232.TechCoef_elec_GRIDR,
                L2232.TechCoef_elecownuse_GRIDR, L2232.Production_imports_GRIDR, L2232.Production_elec_gen_GRIDR,
                L2232.StubTechElecMarket_backup_CHINA)
  } else {
    stop("Unknown command")
  }
}
