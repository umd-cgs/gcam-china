# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L223.electricity_CHINA
#'
#' Generates GCAM-China model inputs for electricity sector by grid regions and provinces.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L223.DeleteSubsector_CHINAelec},
#' \code{L223.Supplysector_CHINAelec}, \code{L223.SubsectorShrwtFllt_CHINAelec}, \code{L223.SubsectorInterp_CHINAelec},
#' \code{L223.SubsectorLogit_CHINAelec}, \code{L223.TechShrwt_CHINAelec}, \code{L223.TechCoef_CHINAelec},
#' \code{L223.Production_CHINAelec},
#' \code{L223.Supplysector_elec_GRIDR}, \code{L223.SubsectorShrwtFllt_elec_GRIDR}, \code{L223.SubsectorInterp_elec_GRIDR},
#' \code{L223.SubsectorLogit_elec_GRIDR}, \code{L223.TechShrwt_elec_GRIDR}, \code{L223.TechCoef_elec_GRIDR},
#' \code{L223.Production_elec_GRIDR}, \code{L223.InterestRate_GRIDR}, \code{L223.Pop_GRIDR}, \code{L223.BaseGDP_GRIDR},
#' \code{L223.LaborForceFillout_GRIDR},\code{L223.Supplysector_elec_CHINA}, \code{L223.ElecReserve_CHINA},
#' \code{L223.SubsectorLogit_elec_CHINA}, \code{L223.SubsectorShrwtFllt_elec_CHINA}, \code{L223.SubsectorShrwt_nuc_CHINA},
#' \code{L223.SubsectorShrwt_renew_CHINA}, \code{L223.SubsectorInterp_elec_CHINA}, \code{L223.SubsectorInterpTo_elec_CHINA},
#' \code{L223.StubTech_elec_CHINA}, \code{L223.StubTechEff_elec_CHINA}, \code{L223.StubTechCapFactor_elec_CHINA},
#' \code{L223.StubTechFixOut_elec_CHINA}, \code{L223.StubTechFixOut_hydro_CHINA}, \code{L223.StubTechProd_elec_CHINA},
#' \code{L223.StubTechMarket_elec_CHINA}, \code{L223.StubTechMarket_backup_CHINA}, \code{L223.StubTechElecMarket_backup_CHINA},
#' \code{L223.StubTechCapFactor_elec_wind_CHINA}, \code{L223.StubTechCapFactor_elec_solar_CHINA}. The corresponding file in the
#' original data system was \code{L223.electricity_CHINA.R} (gcam-CHINA level2).
#' @details This chunk generates input files to create an annualized electricity generation sector for each province
#' and creates the demand for the province-level electricity sectors in the grid regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join matches mutate select semi_join summarise transmute
#' @importFrom tidyr gather spread
#' @author YangLiu Jan 2020 / YangOu Dec 2023

module_gcamchina_L223.electricity_CHINA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "gcam-china/future_hydro_gen_EJ",
             FILE = "gcam-china/nuc_share_weight_assumptions",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A23.globaltech_eff",
             "L114.CapacityFactor_wind_province",
             "L119.CapFacScaler_PV_province",
             "L119.CapFacScaler_CSP_province",
             "L223.Supplysector_elec",
             "L223.ElecReserve",
             "L223.SubsectorLogit_elec",
             "L223.SubsectorShrwtFllt_elec",
             "L223.SubsectorShrwt_nuc",
             "L223.SubsectorShrwt_renew",
             "L223.SubsectorInterp_elec",
             "L223.SubsectorInterpTo_elec",
             "L223.StubTech_elec",
             "L223.StubTechEff_elec",
             "L223.StubTechCapFactor_elec",
             "L223.GlobalIntTechBackup_elec",
             "L1231.in_EJ_province_elec_F_tech",
             "L1231.out_EJ_province_elec_F_tech",
             "L1232.out_EJ_sR_elec_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L223.DeleteSubsector_CHINAelec",
             "L223.Supplysector_CHINAelec",
             "L223.SubsectorShrwtFllt_CHINAelec",
             "L223.SubsectorInterp_CHINAelec",
             "L223.SubsectorLogit_CHINAelec",
             "L223.TechShrwt_CHINAelec",
             "L223.TechCoef_CHINAelec",
             "L223.Production_CHINAelec",
             "L223.Supplysector_elec_GRIDR",
             "L223.SubsectorShrwtFllt_elec_GRIDR",
             "L223.SubsectorInterp_elec_GRIDR",
             "L223.SubsectorLogit_elec_GRIDR",
             "L223.TechShrwt_elec_GRIDR",
             "L223.TechCoef_elec_GRIDR",
             "L223.Production_elec_GRIDR",
             "L223.InterestRate_GRIDR",
             "L223.Pop_GRIDR",
             "L223.BaseGDP_GRIDR",
             "L223.LaborForceFillout_GRIDR",
             "L223.Supplysector_elec_CHINA",
             "L223.ElecReserve_CHINA",
             "L223.SubsectorLogit_elec_CHINA",
             "L223.SubsectorShrwtFllt_elec_CHINA",
             "L223.SubsectorShrwt_nuc_CHINA",
             "L223.SubsectorShrwt_renew_CHINA",
             "L223.SubsectorInterp_elec_CHINA",
             "L223.SubsectorInterpTo_elec_CHINA",
             "L223.StubTech_elec_CHINA",
             "L223.StubTechEff_elec_CHINA",
             "L223.StubTechCapFactor_elec_CHINA",
             "L223.StubTechFixOut_elec_CHINA",
             "L223.StubTechFixOut_hydro_CHINA",
             "L223.StubTechProd_elec_CHINA",
             "L223.StubTechMarket_elec_CHINA",
             "L223.StubTechMarket_backup_CHINA",
             "L223.StubTechElecMarket_backup_CHINA",
             "L223.StubTechCapFactor_elec_wind_CHINA",
             "L223.StubTechCapFactor_elec_solar_CHINA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    grid.region <- Geothermal_Hydrothermal_GWh <- province <- geo_province_noresource <-
      region <- supplysector <- subsector <- technology <- year <- value <-
      sector <- calOutputValue <- fuel <- elec <- share <- avg.share <- pref <-
      share.weight.mult <- share.weight <- market.name <- sector.name <- subsector.name <-
      minicam.energy.input <- calibration <- secondary.output <- stub.technology <-
      capacity.factor <- scaler <- capacity.factor.capital <- . <- NULL  # silence package check notes

    # Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = T)
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs", strip_attributes = T)
    future_hydro_gen_EJ <- get_data(all_data,"gcam-china/future_hydro_gen_EJ", strip_attributes = T)
    nuc_share_weight_assumptions <- get_data(all_data, "gcam-china/nuc_share_weight_assumptions", strip_attributes = T)
    #NREL_us_re_technical_potential <- get_data(all_data, "gcam-china/NREL_us_re_technical_potential")
    A23.globaltech_eff <- get_data(all_data, "energy/A23.globaltech_eff", strip_attributes = T)
    L114.CapacityFactor_wind_province <- get_data(all_data, "L114.CapacityFactor_wind_province", strip_attributes = T)
    L119.CapFacScaler_PV_province <- get_data(all_data, "L119.CapFacScaler_PV_province", strip_attributes = T)
    L119.CapFacScaler_CSP_province <- get_data(all_data, "L119.CapFacScaler_CSP_province", strip_attributes = T)
    L223.Supplysector_elec <- get_data(all_data, "L223.Supplysector_elec", strip_attributes = T)
    L223.ElecReserve <- get_data(all_data, "L223.ElecReserve", strip_attributes = T)
    L223.SubsectorLogit_elec <- get_data(all_data, "L223.SubsectorLogit_elec", strip_attributes = T)
    L223.SubsectorShrwtFllt_elec <- get_data(all_data, "L223.SubsectorShrwtFllt_elec", strip_attributes = T)
    L223.SubsectorShrwt_nuc <- get_data(all_data, "L223.SubsectorShrwt_nuc", strip_attributes = T)
    L223.SubsectorShrwt_renew <- get_data(all_data, "L223.SubsectorShrwt_renew", strip_attributes = T)
    L223.SubsectorInterp_elec <- get_data(all_data, "L223.SubsectorInterp_elec", strip_attributes = T)
    L223.SubsectorInterpTo_elec <- get_data(all_data, "L223.SubsectorInterpTo_elec", strip_attributes = T)
    L223.StubTech_elec <- get_data(all_data, "L223.StubTech_elec", strip_attributes = T)
    L223.StubTechEff_elec <- get_data(all_data, "L223.StubTechEff_elec", strip_attributes = T)
    L223.StubTechCapFactor_elec <- get_data(all_data, "L223.StubTechCapFactor_elec", strip_attributes = T)
    L223.GlobalIntTechBackup_elec <- get_data(all_data, "L223.GlobalIntTechBackup_elec", strip_attributes = T)
    L1231.in_EJ_province_elec_F_tech <- get_data(all_data, "L1231.in_EJ_province_elec_F_tech", strip_attributes = T)
    L1231.out_EJ_province_elec_F_tech <- get_data(all_data, "L1231.out_EJ_province_elec_F_tech", strip_attributes = T)
    L1232.out_EJ_sR_elec_CHINA <- get_data(all_data, "L1232.out_EJ_sR_elec_CHINA", strip_attributes = T)
    #L126.out_EJ_province_td_elec <- get_data(all_data, "L126.out_EJ_province_td_elec")

    # Silence package notes
    fixedOutput <- year2 <- NULL


    # A vector of China grid region names
    province_names_mappings %>%
      select(grid.region) %>%
      unique %>%
      arrange(grid.region) %>%
      unlist ->
      grid.regions

    elec_gen_names <- "electricity"


    # A vector indicating provinces where geothermal electric technologies will not be created
     L1231.out_EJ_province_elec_F_tech %>%
      left_join_error_no_match(province_names_mappings, by = "province") %>%
      filter(value == 0) %>%
      transmute(geo_province_noresource = paste(province, "geothermal", sep = " ")) %>%
      unlist ->
      geo_provinces_noresource


    # gcamchina.USE_REGIONAL_ELEC_MARKETS is TRUE, indicating to resolve electricity demands
    # at the level of the grid regions. The entire loop below produces outputs
    # assoicated with resolving demand at the national level, and is currently disabled.
    # Instead, a set of empty tibbles are produced at the end of this chunk.
    if(!gcamchina.USE_REGIONAL_ELEC_MARKETS) {
      # PART 1: THE CHINA REGION
      # Define the sector(s) that will be used in this code file. Can be one or multiple sectors
      # The subsectors of the existing China electricity sector are deleted.
      # Keeping the supplysector info, incl. reserve margin
      # NOTE: This also removes the rooftop PV subsector of the CHINA elect_td_bld sector
      L223.SubsectorLogit_elec %>%
        select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
        filter(region == gcamchina.REGION) ->
        L223.DeleteSubsector_CHINAelec

      # L223.Supplysector_CHINAelec: supplysector for electricity sector in the China region,
      # including logit exponent between grid regions
      # All of the supplysector information is the same as before, except the logit exponent
      tibble(region = gcamchina.REGION,
             supplysector = elec_gen_names,
             output.unit = "EJ",
             input.unit = "EJ",
             price.unit = "1975$/GJ",
             logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = gcamchina.GRID.REGION_LOGIT,
             logit.type = gcamchina.GRID.REGION_LOGIT_TYPE) %>%
        select(LEVEL2_DATA_NAMES[["Supplysector"]]) ->
        L223.Supplysector_CHINAelec

      # L223.SubsectorShrwtFllt_CHINAelec: subsector (grid region) share-weights in China electricity
      # No need to read in subsector logit exponents, which are applied to the technology competition
      tibble(region = gcamchina.REGION,
             supplysector = elec_gen_names,
             subsector = paste(grid.regions, elec_gen_names, sep = " "),
             year.fillout = min(MODEL_BASE_YEARS),
             share.weight = 1) ->
        L223.SubsectorShrwtFllt_CHINAelec

      # L223.SubsectorInterp_CHINAelec: temporal interpolation of subsector share-weights in China electricity
      L223.SubsectorShrwtFllt_CHINAelec %>%
        select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
        mutate(apply.to = "share-weight",
               from.year = max(MODEL_BASE_YEARS),
               to.year = max(MODEL_YEARS),
               interpolation.function = "fixed") ->
        L223.SubsectorInterp_CHINAelec

      # L223.SubsectorLogit_CHINAelec: logit exponent of subsector in CHINA electricity
      # NOTE: There is only one tech per subsector, so the logit choice does not matter
      L223.SubsectorShrwtFllt_CHINAelec %>%
        select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
        mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
               logit.exponent = gcamchina.GRID.REGION_LOGIT,
               logit.type = gcamchina.GRID.REGION_LOGIT_TYPE) %>%
        select(LEVEL2_DATA_NAMES[["SubsectorLogit"]]) ->
        L223.SubsectorLogit_CHINAelec

      # L223.TechShrwt_CHINAelec: technology share-weights in the China region
      L223.SubsectorShrwtFllt_CHINAelec %>%
        select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
        mutate(technology = subsector) %>%
        repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
        mutate(share.weight = 1) ->
        L223.TechShrwt_CHINAelec

      # L223.TechCoef_CHINAelec: technology coefficients and market names in the China region
      L223.TechShrwt_CHINAelec %>%
        select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
        mutate(minicam.energy.input = supplysector,
               coefficient = 1,
               market.name = substr(technology, 1, nchar(subsector) - nchar(supplysector) - 1)) ->
        L223.TechCoef_CHINAelec

      # L223.Production_CHINAelec: calibrated electricity production in China (consuming output of grid subregions)
      L1232.out_EJ_sR_elec_CHINA %>%
        filter(year %in% MODEL_BASE_YEARS) %>%
        mutate(calOutputValue = round(value, digits = energy.DIGITS_CALOUTPUT)) %>%
        left_join_error_no_match(unique(select(calibrated_techs, sector, supplysector)), by = "sector") %>%
        mutate(subsector = paste(grid.region, supplysector, sep = " ")) ->
        L223.out_EJ_sR_elec

      L223.TechCoef_CHINAelec %>%
        select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
        filter(year %in% MODEL_BASE_YEARS) %>%
        left_join_error_no_match(L223.out_EJ_sR_elec, by = c("supplysector", "subsector", "year")) %>%
        mutate(share.weight.year = year,
               tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
        set_subsector_shrwt() %>%
        select(LEVEL2_DATA_NAMES[["Production"]]) ->
        L223.Production_CHINAelec
    }

    # PART 2: THE GRIDR REGIONS
    # NOTE: GRIDR grid regions function in similar fashion to the China region:
    # competing electricity from subregions

    # L223.Supplysector_elec_GRIDR: supplysector for electricity sector in the grid regions,
    # including logit exponent between provinces within grid region
    # NOTE: use the same logit exponent for provinces within GRIDR region as for GRIDR regions within China
    tibble(region = grid.regions,
           supplysector = elec_gen_names,
           output.unit = "EJ",
           input.unit = "EJ",
           price.unit = "1975$/GJ",
           logit.year.fillout = min(MODEL_BASE_YEARS),
           logit.exponent = gcamchina.GRID.REGION_LOGIT,
           logit.type = gcamchina.GRID.REGION_LOGIT_TYPE) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME)) ->
      L223.Supplysector_elec_GRIDR

    # L223.SubsectorShrwtFllt_elec_GRIDR: subsector (grid region) share-weights in grid regions
    province_names_mappings %>%
      select(region = grid.region, province) %>%
      mutate(supplysector = elec_gen_names,
             subsector = paste(province, supplysector, sep = " "),
             year.fillout = min(MODEL_BASE_YEARS),
             share.weight = 1) %>%
      select(-province) %>%
      arrange(region) ->
      L223.SubsectorShrwtFllt_elec_GRIDR

    # L223.SubsectorInterp_elec_GRIDR: temporal interpolation of subsector (grid region) share-weights in grid regions
    L223.SubsectorShrwtFllt_elec_GRIDR %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") ->
      L223.SubsectorInterp_elec_GRIDR

    # L223.SubsectorLogit_elec_GRIDR: logit exponent of subsector (grid regions) in grid regions
    # NOTE: There is only one tech per subsector, so the logit choice does not matter
    L223.SubsectorShrwtFllt_elec_GRIDR %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = gcamchina.GRID.REGION_LOGIT,
             logit.type = gcamchina.GRID.REGION_LOGIT_TYPE) %>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)) ->
      L223.SubsectorLogit_elec_GRIDR

    # L223.TechShrwt_elec_GRIDR: technology share-weights in grid regions
    L223.SubsectorShrwtFllt_elec_GRIDR %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(technology = subsector) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1) ->
      L223.TechShrwt_elec_GRIDR

    # L223.TechCoef_elec_GRIDR: technology coefficients and market names in grid regions
    L223.TechShrwt_elec_GRIDR %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
      mutate(minicam.energy.input = supplysector,
             coefficient = 1,
             market.name = substr(technology, 1, nchar(subsector) - nchar(supplysector) - 1)) ->
      L223.TechCoef_elec_GRIDR

    # L223.Production_elec_GRIDR: calibrated electricity production in grid region (consuming output of grid subregions)
    L1231.out_EJ_province_elec_F_tech %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = round(value, digits = energy.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(unique(select(calibrated_techs, sector, supplysector)), by = "sector") %>%
      mutate(subsector = paste(province, supplysector, sep = " ")) %>%
      # This needs to be aggregated to the subsector level
      group_by(supplysector, subsector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup ->
      L223.out_EJ_province_elec

    L223.TechCoef_elec_GRIDR %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L223.out_EJ_province_elec, by = c("supplysector", "subsector", "year")) %>%
      mutate(share.weight.year = year,
             # tech.share.weights are set at technology level
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      # sub.share.weights are set the the subsector level in case with multiple technologies
      set_subsector_shrwt() %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L223.Production_elec_GRIDR

    # Socioeconomic information in the electricity grid regions (required for GCAM to run with these regions)

    # L223.InterestRate_GRIDR: Interest rates in the GRIDR grid regions
    tibble(region = grid.regions,
           interest.rate = socioeconomics.DEFAULT_INTEREST_RATE) ->
      L223.InterestRate_GRIDR

    # L223.Pop_GRIDR: Population
    tibble(region = grid.regions,
           totalPop = 1) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      L223.Pop_GRIDR

    # L223.BaseGDP_GRIDR: Base GDP in GRIDR grid regions
    tibble(region = grid.regions,
           baseGDP = 1)  ->
      L223.BaseGDP_GRIDR

    # L223.LaborForceFillout_GRIDR: labor force in the grid regions
    tibble(region = grid.regions,
           year.fillout = min(MODEL_BASE_YEARS),
           laborforce = socioeconomics.DEFAULT_LABORFORCE) ->
      L223.LaborForceFillout_GRIDR


    # PART 3: THE PROVINCES
    # All tables for which processing is identical are done by a function.
    # This applies to the supplysectors, subsectors, and stub tech characteristics of the provinces
    process_CHINA_to_provinces <- function(data, noHKMC = TRUE) {
      province <- region <- grid.region <- subsector <- market.name <-
        minicam.energy.input <- NULL  # silence package check notes

      if(noHKMC) {
        data_new <- data %>%
          filter(region == gcamchina.REGION) %>%
          write_to_all_provinces(names(data),gcamchina.PROVINCES_NOHKMC)
      } else {
        data_new <- data %>%
          filter(region == gcamchina.REGION) %>%
          write_to_all_provinces(names(data),gcamchina.PROVINCES_ALL)
      }

      if("subsector" %in% names(data_new)) {
        data_new <- data_new %>%
          filter(!paste(region, subsector) %in% geo_provinces_noresource)
      }

      # Re-set markets from China to regional markets, if called for in the GCAM-China assumptions for selected fuels
      if(gcamchina.USE_REGIONAL_FUEL_MARKETS & "market.name" %in% names(data_new)) {
        data_new <- data_new %>%
          left_join_error_no_match(select(province_names_mappings,province, grid.region), by = c("region" = "province")) %>%
          mutate(market.name = replace(market.name, minicam.energy.input %in% gcamchina.REGIONAL_FUEL_MARKETS,
                                       grid.region[minicam.energy.input %in% gcamchina.REGIONAL_FUEL_MARKETS])) %>%
          select(-grid.region)
      }

      data_new
    }

    # logit tables need to include HK and MC
    process_CHINA_to_provinces(L223.Supplysector_elec, noHKMC = FALSE) -> L223.Supplysector_elec_CHINA
    process_CHINA_to_provinces(L223.SubsectorLogit_elec, noHKMC = FALSE) -> L223.SubsectorLogit_elec_CHINA
    process_CHINA_to_provinces(L223.ElecReserve, noHKMC = FALSE) -> L223.ElecReserve_CHINA

    process_CHINA_to_provinces(L223.SubsectorShrwtFllt_elec) -> L223.SubsectorShrwtFllt_elec_CHINA
    process_CHINA_to_provinces(L223.SubsectorShrwt_nuc) -> L223.SubsectorShrwt_nuc_CHINA
    process_CHINA_to_provinces(L223.SubsectorShrwt_renew) -> L223.SubsectorShrwt_renew_CHINA
    process_CHINA_to_provinces(L223.SubsectorInterp_elec) -> L223.SubsectorInterp_elec_CHINA
    process_CHINA_to_provinces(L223.SubsectorInterpTo_elec) -> L223.SubsectorInterpTo_elec_CHINA
    process_CHINA_to_provinces(L223.StubTech_elec) -> L223.StubTech_elec_CHINA
    process_CHINA_to_provinces(L223.StubTechEff_elec) -> L223.StubTechEff_elec_CHINA
    process_CHINA_to_provinces(L223.StubTechCapFactor_elec) -> L223.StubTechCapFactor_elec_CHINA

    # NOTE: Modify the share-weight path for nuclear to include province preferences
    nuc_share_weight_assumptions %>%
      gather("year","share.weight",-region) %>%
      mutate(supplysector = "electricity",
             subsector = "nuclear",
             year = as.numeric(year)) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwt"]]) ->
      L223.SubsectorShrwt_nuc_CHINA

    # Stub technology information for province electricity generation
    # calibration
    L1231.in_EJ_province_elec_F_tech %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calibrated.value = round(value, digits = energy.DIGITS_CALOUTPUT),
             region = province) %>%
      left_join_error_no_match(select(calibrated_techs, -minicam.energy.input, -secondary.output),
                               by = c("sector", "fuel", "technology")) %>%
      mutate(stub.technology = technology) %>%
      filter(calibration == "input") ->
      L223.in_EJ_province_elec_F_tech

    # NOTE: Fixed output is assumed to apply in all historical years, regardless of final calibration year
    L1231.out_EJ_province_elec_F_tech %>%
      filter(year %in% MODEL_YEARS, year %in% HISTORICAL_YEARS) %>%
      mutate(calOutputValue = round(value, digits = energy.DIGITS_CALOUTPUT),
             region = province) %>%
      left_join_error_no_match(select(calibrated_techs, -minicam.energy.input, -secondary.output),
                               by = c("sector", "fuel", "technology")) %>%
      mutate(stub.technology = technology) ->
      L223.out_EJ_province_elec_F_tech

    L223.out_EJ_province_elec_F_tech %>%
      filter(calibration == "fixed output") ->
      L223.fixout_EJ_province_elec_F_tech

    L223.out_EJ_province_elec_F_tech %>%
      filter(calibration != "fixed output") ->
      L223.calout_EJ_province_elec_F_tech

    # L223.StubTechFixOut_elec_CHINA: fixed output of electricity generation technologies
    L223.fixout_EJ_province_elec_F_tech %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], calOutputValue) %>%
      mutate(fixedOutput = round(calOutputValue, digits = energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             subs.share.weight = 0,
             tech.share.weight = 0) %>%
      select(-calOutputValue) ->
      L223.StubTechFixOut_elec_CHINA

    # Add in future hydropower generation here
    # L223.StubTechFixOut_hydro_CHINA: fixed output of future hydropower
    # NOTE: National data is adjusted in 2014 and 2020 to match data from Bo Liu;
    # Downscare to provinces is based on relative hydro ele generation share in 2010
    # Linear interpolate rule 2
    future_hydro_gen_EJ %>%
      complete(year = FUTURE_YEARS) %>%
      mutate(value = approx_fun(year,value,rule = 2)) %>%
      filter(year %in% MODEL_FUTURE_YEARS) ->
      hydro_elec_gen

    L223.StubTechFixOut_elec_CHINA %>%
      filter(grepl("hydro", stub.technology), year == max(HISTORICAL_YEARS)) %>%
      mutate(share = fixedOutput / sum(fixedOutput)) %>%
      as_tibble() %>%
      repeat_add_columns(tibble(year2 = MODEL_FUTURE_YEARS)) %>%
      mutate(year = year2,share.weight.year = year2) %>%
      left_join(hydro_elec_gen,-value,by = "year") %>%
      mutate(fixedOutput = value * share) ->
      L223.StubTechFixOut_hydro_CHINA

    L223.StubTechFixOut_hydro_CHINA[LEVEL2_DATA_NAMES[["StubTechFixOut"]]] ->
      L223.StubTechFixOut_hydro_CHINA

    # L223.StubTechProd_elec_CHINA: calibrated output of electricity generation technologies
    L223.calout_EJ_province_elec_F_tech %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], calOutputValue) %>%
      mutate(share.weight.year = year) %>%
      set_subsector_shrwt %>%
      mutate(share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      filter(!paste(region, subsector) %in% geo_provinces_noresource) ->
      L223.StubTechProd_elec_CHINA

    # L223.StubTechMarket_elec_CHINA: market names of inputs to province electricity sectors
    L223.StubTech_elec_CHINA %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # For rooftop_pv (technology), match in distributed_solar instead of backup_electricity (minicam.energy.input)
      left_join_keep_first_only(select(A23.globaltech_eff, supplysector, subsector, technology, minicam.energy.input),
                                by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      # Remove NA rows for hydro
      na.omit %>%
      mutate(market.name = gcamchina.REGION,
             market.name = replace(market.name,
                                   minicam.energy.input %in% c(gcamchina.PROVINCE_RENEWABLE_RESOURCES, gcamchina.PROVINCE_UNLIMITED_RESOURCES),
                                   region[minicam.energy.input %in% c(gcamchina.PROVINCE_RENEWABLE_RESOURCES, gcamchina.PROVINCE_UNLIMITED_RESOURCES)])) %>%
      filter(!paste(region, subsector) %in% geo_provinces_noresource) ->
      L223.StubTechMarket_elec_CHINA

    if(gcamchina.USE_REGIONAL_FUEL_MARKETS) {
      L223.StubTechMarket_elec_CHINA %>%
        left_join_error_no_match(select(province_names_mappings, grid.region, province), by = c("region" = "province")) %>%
        mutate(market.name = replace(market.name, minicam.energy.input %in% gcamchina.REGIONAL_FUEL_MARKETS,
                                     region[minicam.energy.input %in% gcamchina.REGIONAL_FUEL_MARKETS])) %>%
        select(-grid.region) ->
        L223.StubTechMarket_elec_CHINA
    }

    # L223.StubTechMarket_backup_CHINA: market names of backup inputs to province electricity sectors
    L223.GlobalIntTechBackup_elec %>%
      mutate(supplysector = sector.name, subsector = subsector.name) %>%
      write_to_all_provinces(names = c(names(.),'region'),gcamchina.PROVINCES_NOHKMC) %>%
      mutate(market.name = gcamchina.REGION, stub.technology = technology) %>%
      select(LEVEL2_DATA_NAMES[["StubTechMarket"]]) ->
      L223.StubTechMarket_backup_CHINA

    # L223.StubTechElecMarket_backup_CHINA: market name of electricity sector for backup calculations
    # The backup electric market is only set here if regional electricity markets are not used (i.e. one national grid)
    if(!gcamchina.USE_REGIONAL_ELEC_MARKETS) {
      L223.StubTechMarket_backup_CHINA %>%
        select(LEVEL2_DATA_NAMES[["StubTechYr"]]) %>%
        mutate(electric.sector.market = gcamchina.REGION) ->
        L223.StubTechElecMarket_backup_CHINA
    }

    # L223.StubTechCapFactor_elec_wind_CHINA: capacity factors for wind electricity in the provinces
    # Just use the subsector for matching - technologies include storage technologies as well
    L114.CapacityFactor_wind_province %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector, subsector),
                               by = c("sector", "fuel")) ->
      L223.CapacityFactor_wind_province

    L223.StubTechCapFactor_elec %>%
      filter(region == gcamchina.REGION) %>%
      semi_join(L223.CapacityFactor_wind_province, by = c("supplysector", "subsector")) %>%
      select(-region, -capacity.factor) %>%
      write_to_all_provinces(names = c(names(.),'region'),gcamchina.PROVINCES_NOHKMC) %>%
      left_join(L223.CapacityFactor_wind_province,
                               by = c("region" = "province", "supplysector", "subsector")) %>%
      mutate(capacity.factor = round(capacity.factor, digits = energy.DIGITS_CAPACITY_FACTOR)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCapFactor"]]) ->
      L223.StubTechCapFactor_elec_wind_CHINA

    # L223.StubTechCapFactor_elec_solar_CHINA: capacity factors by province and solar electric technology
    L119.CapFacScaler_PV_province %>%
      bind_rows(L119.CapFacScaler_CSP_province) %>%
      left_join_error_no_match(select(calibrated_techs, sector, fuel, supplysector, subsector, technology),
                               by = c("sector", "fuel")) ->
      L223.CapFacScaler_solar_province

    # Just use the subsector for matching - technologies include storage technologies as well
    L223.StubTechCapFactor_elec %>%
      filter(region == gcamchina.REGION) %>%
      semi_join(L223.CapFacScaler_solar_province, by = c("supplysector", "subsector")) %>%
      select(-region) %>%
      write_to_all_provinces(.,names = c(names(.),'region'),gcamchina.PROVINCES_NOHKMC) %>%
      # For matching capacity factors to technologies, create a variable (tech) that matches what's in the capacity factor table
      mutate(tech = sub("_storage", "", stub.technology)) %>%
      left_join_error_no_match(L223.CapFacScaler_solar_province,
                               by = c("region" = "province", "supplysector", "subsector", "tech" = "technology")) %>%
      mutate(capacity.factor = round(capacity.factor * scaler, digits = energy.DIGITS_COST)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCapFactor"]]) ->
      L223.StubTechCapFactor_elec_solar_CHINA

    # Produce outputs

    if(exists("L223.DeleteSubsector_CHINAelec")) {
      L223.DeleteSubsector_CHINAelec %>%
        add_title("Define the electricity sector(s) in the China region") %>%
        add_units("NA") %>%
        add_comments("The file is generated if to resolve electricity demands at the US national level") %>%
        add_comments("The subsectors of the existing China electricity sector are deleted") %>%
        add_legacy_name("L223.DeleteSubsector_CHINAelec") %>%
        add_precursors("L223.SubsectorLogit_elec") ->
        L223.DeleteSubsector_CHINAelec
    } else {
      # If gcamchina.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      missing_data() %>%
        add_legacy_name("L223.DeleteSubsector_CHINAelec") ->
        L223.DeleteSubsector_CHINAelec
    }

    if(exists("L223.Supplysector_CHINAelec")) {
      L223.Supplysector_CHINAelec %>%
        add_title("Supplysector for electricity sector in the China region") %>%
        add_units("NA") %>%
        add_comments("The file is generated if resolving electricity demands at the China national level") %>%
        add_comments("All of the supplysector information is the same as before") %>%
        add_comments("except including logit exponent between grid regions")
      add_legacy_name("L223.Supplysector_CHINAelec") ->
        L223.Supplysector_CHINAelec
    } else {
      # If gcamchina.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      missing_data() %>%
        add_legacy_name("L223.Supplysector_CHINAelec") ->
        L223.Supplysector_CHINAelec
    }

    if(exists("L223.SubsectorShrwtFllt_CHINAelec")) {
      L223.SubsectorShrwtFllt_CHINAelec %>%
        add_title("Subsector (grid region) share-weights in China electricity") %>%
        add_units("Unitless") %>%
        add_comments("The file is generated if resolving electricity demands at the China national level") %>%
        add_legacy_name("L223.SubsectorShrwtFllt_CHINAelec") %>%
        add_precursors("gcam-china/province_names_mappings") ->
        L223.SubsectorShrwtFllt_CHINAelec
    } else {
      # If gcamchina.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      missing_data() %>%
        add_legacy_name("L223.SubsectorShrwtFllt_CHINAelec") ->
        L223.SubsectorShrwtFllt_CHINAelec
    }

    if(exists("L223.SubsectorInterp_CHINAelec")) {
      L223.SubsectorInterp_CHINAelec %>%
        add_title("Table headers for temporal interpolation of subsector (grid region) share-weights") %>%
        add_units("Unitless") %>%
        add_comments("The file is generated if resolving electricity demands at the China national level") %>%
        add_legacy_name("L223.SubsectorInterp_CHINAelec") %>%
        same_precursors_as("L223.SubsectorShrwtFllt_CHINAelec") ->
        L223.SubsectorInterp_CHINAelec
    } else {
      # If gcamchina.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      missing_data() %>%
        add_legacy_name("L223.SubsectorInterp_CHINAelec") ->
        L223.SubsectorInterp_CHINAelec
    }

    if(exists("L223.SubsectorLogit_CHINAelec")) {
      L223.SubsectorLogit_CHINAelec %>%
        add_title("Logit exponent of subsectors (grid regions) in China electricity") %>%
        add_units("Unitless") %>%
        add_comments("The file is generated if resolving electricity demands at the China national level") %>%
        add_comments("There is only one tech per subsector, so the logit choice does not matter") %>%
        add_legacy_name("L223.SubsectorLogit_CHINAelec") %>%
        same_precursors_as("L223.SubsectorShrwtFllt_CHINAelec") ->
        L223.SubsectorLogit_CHINAelec
    } else {
      # If gcamchina.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      missing_data() %>%
        add_legacy_name("L223.SubsectorLogit_CHINAelec") ->
        L223.SubsectorLogit_CHINAelec
    }

    if(exists("L223.TechShrwt_CHINAelec")) {
      L223.TechShrwt_CHINAelec %>%
        add_title("Technology share-weights in the China region") %>%
        add_units("Unitless") %>%
        add_comments("The file is generated if resolving electricity demands at the China national level") %>%
        add_legacy_name("L223.TechShrwt_CHINAelec") %>%
        same_precursors_as("L223.SubsectorShrwtFllt_CHINAelec") ->
        L223.TechShrwt_CHINAelec
    } else {
      # If gcamchina.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      missing_data() %>%
        add_legacy_name("L223.TechShrwt_CHINAelec") ->
        L223.TechShrwt_CHINAelec
    }

    if(exists("L223.TechCoef_CHINAelec")) {
      L223.TechCoef_CHINAelec %>%
        add_title("Technology coefficients and market names in the China region") %>%
        add_units("Unitless") %>%
        add_comments("The file is generated if resolving electricity demands at the China national level") %>%
        add_legacy_name("L223.TechCoef_CHINAelec") %>%
        same_precursors_as("L223.TechShrwt_CHINAelec") ->
        L223.TechCoef_CHINAelec
    } else {
      # If gcamchina.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      missing_data() %>%
        add_legacy_name("L223.TechCoef_CHINAelec") ->
        L223.TechCoef_CHINAelec
    }

    if(exists("L223.Production_CHINAelec")) {
      L223.Production_CHINAelec %>%
        add_title("Calibration electricity production in the China region") %>%
        add_units("EJ") %>%
        add_comments("The file is generated if resolving electricity demands at the China national level") %>%
        add_legacy_name("L223.Production_CHINAelec") %>%
        add_precursors("L1232.out_EJ_sR_elec_CHINA",
                       "energy/calibrated_techs") ->
        L223.Production_CHINAelec
    } else {
      # If gcamchina.USE_REGIONAL_ELEC_MARKETS is TURE,
      # indicating to resolve electricity demands at the level of the grid regions,
      # then blank tibbles of the national level data are produced.
      missing_data() %>%
        add_legacy_name("L223.Production_CHINAelec") %>%
        add_precursors("L1232.out_EJ_sR_elec_CHINA") ->
        L223.Production_CHINAelec
    }

    L223.Supplysector_elec_GRIDR %>%
      add_title("Supplysector information for electricity sector in the grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Include logit exponent between provinces") %>%
      add_comments("Use the same logit exponent for provinces within GRIDR region as for GRIDR regions within China") %>%
      add_legacy_name("L223.Supplysector_elec_GRIDR") %>%
      add_precursors("gcam-china/province_names_mappings") ->
      L223.Supplysector_elec_GRIDR

    L223.SubsectorShrwtFllt_elec_GRIDR %>%
      add_title("Subsector (province) share-weights in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Set share-weights for provinces within grid region") %>%
      add_legacy_name("L223.SubsectorShrwtFllt_elec_GRIDR") %>%
      add_precursors("gcam-china/province_names_mappings") ->
      L223.SubsectorShrwtFllt_elec_GRIDR

    L223.SubsectorInterp_elec_GRIDR %>%
      add_title("Table header for temporal interpolation of subsector (province) share-weights in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Set up temporal interterpolation of province share-weights within grid region") %>%
      add_legacy_name("L223.SubsectorInterp_elec_GRIDR") %>%
      same_precursors_as("L223.SubsectorShrwtFllt_elec_GRIDR") ->
      L223.SubsectorInterp_elec_GRIDR

    L223.SubsectorLogit_elec_GRIDR %>%
      add_title("Logit exponent of subsector (provinces) in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("There is only one tech per subsector, so the logit choice does not matter") %>%
      add_legacy_name("L223.SubsectorLogit_elec_GRIDR") %>%
      same_precursors_as("L223.SubsectorShrwtFllt_elec_GRIDR") ->
      L223.SubsectorLogit_elec_GRIDR

    L223.TechShrwt_elec_GRIDR %>%
      add_title("Technology share-weights in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Technology is the same as subsector within grid region") %>%
      add_legacy_name("L223.TechShrwt_elec_GRIDR") %>%
      same_precursors_as("L223.SubsectorShrwtFllt_elec_GRIDR") ->
      L223.TechShrwt_elec_GRIDR

    L223.TechCoef_elec_GRIDR %>%
      add_title("Technology coefficients and market names in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Technology is the same as subsector within grid region") %>%
      add_comments("Market name is province name") %>%
      add_legacy_name("L223.TechCoef_elec_GRIDR") %>%
      same_precursors_as("L223.TechShrwt_elec_GRIDR") ->
      L223.TechCoef_elec_GRIDR

    L223.Production_elec_GRIDR %>%
      add_title("Calibrated electricity production of subsectors in grid regions") %>%
      add_units("EJ") %>%
      add_comments("Subsector share-weight is zero if production of all technologies in the subsector is zero") %>%
      add_comments("Technology share-weight is zero if production of the technology is zero") %>%
      add_legacy_name("L223.Production_elec_GRIDR") %>%
      add_precursors("L1231.out_EJ_province_elec_F_tech",
                     "energy/calibrated_techs") ->
      L223.Production_elec_GRIDR

    L223.InterestRate_GRIDR %>%
      add_title("Interest rates in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Use the default interest rate") %>%
      add_legacy_name("L223.InterestRate_GRIDR") %>%
      add_precursors("gcam-china/province_names_mappings") ->
      L223.InterestRate_GRIDR

    L223.Pop_GRIDR %>%
      add_title("Population in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("The same value is copied to all model years") %>%
      add_legacy_name("L223.Pop_GRIDR") %>%
      add_precursors("gcam-china/province_names_mappings") ->
      L223.Pop_GRIDR

    L223.BaseGDP_GRIDR %>%
      add_title("Base GDP in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("") %>%
      add_legacy_name("L223.BaseGDP_GRIDR") %>%
      add_precursors("gcam-china/province_names_mappings") ->
      L223.BaseGDP_GRIDR

    L223.LaborForceFillout_GRIDR %>%
      add_title("Labor force in grid regions") %>%
      add_units("Unitless") %>%
      add_comments("Use the default labor force") %>%
      add_legacy_name("L223.LaborForceFillout_GRIDR") %>%
      add_precursors("gcam-china/province_names_mappings") ->
      L223.LaborForceFillout_GRIDR

    L223.Supplysector_elec_CHINA %>%
      add_title("Supplysector information of electricity sector in the provinces") %>%
      add_units("Unitless") %>%
      add_comments("The same China region values are repeated for each province") %>%
      add_legacy_name("L223.Supplysector_elec_CHINA") %>%
      add_precursors("L223.Supplysector_elec") ->
      L223.Supplysector_elec_CHINA

    L223.ElecReserve_CHINA %>%
      add_title("Electricity reserve margin and average grid capacity factor in the provinces") %>%
      add_units("Unitless") %>%
      add_comments("The same China region values are repeated for each province") %>%
      add_legacy_name("L223.ElecReserve_CHINA") %>%
      add_precursors("L223.ElecReserve") ->
      L223.ElecReserve_CHINA

    L223.SubsectorLogit_elec_CHINA %>%
      add_title("Logit exponent of subsectors (fuels) in the provinces") %>%
      add_units("Unitless") %>%
      add_comments("The same China region values are repeated for each province") %>%
      add_comments("provinces with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.SubsectorLogit_elec_CHINA") %>%
      add_precursors("L223.SubsectorLogit_elec") ->
      L223.SubsectorLogit_elec_CHINA

    L223.SubsectorShrwtFllt_elec_CHINA %>%
      add_title("Subsector (fuel) share-weights in the provinces") %>%
      add_units("Unitless") %>%
      add_comments("The same China region values are repeated for each province") %>%
      add_comments("provinces with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.SubsectorShrwtFllt_elec_CHINA") %>%
      add_precursors("L223.SubsectorShrwtFllt_elec") ->
      L223.SubsectorShrwtFllt_elec_CHINA

    L223.SubsectorShrwt_nuc_CHINA %>%
      add_title("Share-weights for nuclear in the provinces") %>%
      add_units("Unitless") %>%
      add_comments("Table used to adjust nuclear share weights that have been adjusted") %>%
      add_comments("manually to match data from Bo Liu") %>%
      add_legacy_name("L223.SubsectorShrwt_nuc_CHINA") %>%
      add_precursors("L223.SubsectorShrwt_nuc",
                     "gcam-china/nuc_share_weight_assumptions") ->
      L223.SubsectorShrwt_nuc_CHINA

    L223.SubsectorShrwt_renew_CHINA %>%
      add_title("Share-weights for renewable energy in the provinces") %>%
      add_units("Unitless") %>%
      add_comments("The same China region values are repeated for each province") %>%
      add_comments("provinces with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.SubsectorShrwt_renew_CHINA") %>%
      add_precursors("L223.SubsectorShrwt_renew") ->
      L223.SubsectorShrwt_renew_CHINA

    L223.SubsectorInterp_elec_CHINA %>%
      add_title("Temporal (2100) interpolation of subsectors (fuels) in the provinces") %>%
      add_units("Unitless") %>%
      add_comments("The same China region values are repeated for each province") %>%
      add_comments("provinces with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.SubsectorInterp_elec_CHINA") %>%
      add_precursors("L223.SubsectorInterp_elec") ->
      L223.SubsectorInterp_elec_CHINA

    L223.SubsectorInterpTo_elec_CHINA %>%
      add_title("Temporal (2300) interpolation of subsectors (fuels) in the provinces") %>%
      add_units("Unitless") %>%
      add_comments("The same China region values are repeated for each province") %>%
      add_comments("provinces with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.SubsectorInterpTo_elec_CHINA") %>%
      add_precursors("L223.SubsectorInterpTo_elec") ->
      L223.SubsectorInterpTo_elec_CHINA

    L223.StubTech_elec_CHINA %>%
      add_title("Stub technology information for electricity generation in the provinces") %>%
      add_units("Unitless") %>%
      add_comments("The same China region values are repeated for each province") %>%
      add_comments("provinces with no geothermal resource are deleted for the subsector") %>%
      add_legacy_name("L223.StubTech_elec_CHINA") %>%
      add_precursors("L223.StubTech_elec") ->
      L223.StubTech_elec_CHINA

    L223.StubTechEff_elec_CHINA %>%
      add_title("Stub technology efficiency for electricity generation in the provinces") %>%
      add_units("Unitless") %>%
      add_comments("The same China region values are repeated for each province") %>%
      add_comments("Re-set markets from China to regional grid markets for selected fuels") %>%
      add_legacy_name("L223.StubTechEff_elec_CHINA") %>%
      add_precursors("L223.StubTechEff_elec") ->
      L223.StubTechEff_elec_CHINA

    L223.StubTechCapFactor_elec_CHINA %>%
      add_title("Capacity factor of stub technology for electricity generation in the provinces") %>%
      add_units("Unitless") %>%
      add_comments("The same China region values are repeated for each province") %>%
      add_legacy_name("L223.StubTechCapFactor_elec_CHINA") %>%
      add_precursors("L223.StubTechCapFactor_elec") ->
      L223.StubTechCapFactor_elec_CHINA

    L223.StubTechFixOut_elec_CHINA %>%
      add_title("Fixed outputs of stub technology electricity generation in the provinces") %>%
      add_units("EJ") %>%
      add_comments("Applied to historical model years") %>%
      add_legacy_name("L223.StubTechFixOut_elec_CHINA") %>%
      add_precursors("L1231.out_EJ_province_elec_F_tech",
                     "energy/calibrated_techs") ->
      L223.StubTechFixOut_elec_CHINA

    L223.StubTechFixOut_hydro_CHINA %>%
      add_title("Fixed outputs of future hydropower electricity generation in the provinces") %>%
      add_units("EJ") %>%
      add_comments("It holds constants beyond 2020, and interpolate between 2014 and 2020.") %>%
      add_legacy_name("L223.StubTechFixOut_hydro_CHINA") %>%
      add_precursors("L1231.out_EJ_province_elec_F_tech",
                     "energy/calibrated_techs",
                     "gcam-china/future_hydro_gen_EJ") ->
      L223.StubTechFixOut_hydro_CHINA

    L223.StubTechProd_elec_CHINA %>%
      add_title("Calibrated outputs of electricity stub technology in the provinces") %>%
      add_units("EJ") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L223.StubTechProd_elec_CHINA") %>%
      add_precursors("L1231.in_EJ_province_elec_F_tech",
                     "L1231.out_EJ_province_elec_F_tech",
                     "energy/calibrated_techs") ->
      L223.StubTechProd_elec_CHINA

    L223.StubTechMarket_elec_CHINA %>%
      add_title("Market names of inputs to province electricity sectors") %>%
      add_units("Unitless") %>%
      add_comments("Re-set markets from China to regional grid markets for selected fuels") %>%
      add_legacy_name("L223.StubTechMarket_elec_CHINA") %>%
      same_precursors_as("L223.StubTech_elec_CHINA") %>%
      add_precursors("energy/A23.globaltech_eff") ->
      L223.StubTechMarket_elec_CHINA

    L223.StubTechMarket_backup_CHINA %>%
      add_title("Market names of backup inputs to province electricity sectors") %>%
      add_units("Unitless") %>%
      add_comments("Set market as China") %>%
      add_legacy_name("L223.StubTechMarket_backup_CHINA") %>%
      add_precursors("L223.GlobalIntTechBackup_elec",
                     "gcam-china/province_names_mappings") ->
      L223.StubTechMarket_backup_CHINA

    if(exists("L223.StubTechElecMarket_backup_CHINA")) {
      L223.StubTechElecMarket_backup_CHINA %>%
        add_title("Market name of electricity sector for backup calculations") %>%
        add_units("Unitless") %>%
        add_comments("The backup electric market is only set here if regional electricity markets are not used") %>%
        add_legacy_name("L223.StubTechElecMarket_backup_CHINA") %>%
        same_precursors_as("L223.StubTechMarket_backup_CHINA") ->
        L223.StubTechElecMarket_backup_CHINA
    } else {
      # If regional electricity markets are not used,
      # then a blank tibble of the backup electric market is produced.
      missing_data() %>%
        add_legacy_name("L223.StubTechElecMarket_backup_CHINA") ->
        L223.StubTechElecMarket_backup_CHINA
    }

    L223.StubTechCapFactor_elec_wind_CHINA %>%
      add_title("Capacity factors for wind electricity in the provinces") %>%
      add_units("Unitless") %>%
      add_comments("Include storage technologies as well") %>%
      add_legacy_name("L223.StubTechCapFactor_elec_wind_CHINA") %>%
      add_precursors("L114.CapacityFactor_wind_province",
                     "energy/calibrated_techs",
                     "gcam-china/province_names_mappings") ->
      L223.StubTechCapFactor_elec_wind_CHINA

    L223.StubTechCapFactor_elec_solar_CHINA %>%
      add_title("Capacity factors for solar electricity in the provinces") %>%
      add_units("Unitless") %>%
      add_comments("Include storage technologies as well") %>%
      add_legacy_name("L223.StubTechCapFactor_elec_solar_CHINA") %>%
      add_precursors("L119.CapFacScaler_PV_province",
                     "L119.CapFacScaler_CSP_province",
                     "energy/calibrated_techs",
                     "gcam-china/province_names_mappings") ->
      L223.StubTechCapFactor_elec_solar_CHINA

    return_data(L223.DeleteSubsector_CHINAelec,
                L223.Supplysector_CHINAelec,
                L223.SubsectorShrwtFllt_CHINAelec,
                L223.SubsectorInterp_CHINAelec,
                L223.SubsectorLogit_CHINAelec,
                L223.TechShrwt_CHINAelec,
                L223.TechCoef_CHINAelec,
                L223.Production_CHINAelec,
                L223.Supplysector_elec_GRIDR,
                L223.SubsectorShrwtFllt_elec_GRIDR,
                L223.SubsectorInterp_elec_GRIDR,
                L223.SubsectorLogit_elec_GRIDR,
                L223.TechShrwt_elec_GRIDR,
                L223.TechCoef_elec_GRIDR,
                L223.Production_elec_GRIDR,
                L223.InterestRate_GRIDR,
                L223.Pop_GRIDR,
                L223.BaseGDP_GRIDR,
                L223.LaborForceFillout_GRIDR,
                L223.Supplysector_elec_CHINA,
                L223.ElecReserve_CHINA,
                L223.SubsectorLogit_elec_CHINA,
                L223.SubsectorShrwtFllt_elec_CHINA,
                L223.SubsectorShrwt_nuc_CHINA,
                L223.SubsectorShrwt_renew_CHINA,
                L223.SubsectorInterp_elec_CHINA,
                L223.SubsectorInterpTo_elec_CHINA,
                L223.StubTech_elec_CHINA,
                L223.StubTechEff_elec_CHINA,
                L223.StubTechCapFactor_elec_CHINA,
                L223.StubTechFixOut_elec_CHINA,
                L223.StubTechFixOut_hydro_CHINA,
                L223.StubTechProd_elec_CHINA,
                L223.StubTechMarket_elec_CHINA,
                L223.StubTechMarket_backup_CHINA,
                L223.StubTechElecMarket_backup_CHINA,
                L223.StubTechCapFactor_elec_wind_CHINA,
                L223.StubTechCapFactor_elec_solar_CHINA)
  } else {
    stop("Unknown command")
  }
}

