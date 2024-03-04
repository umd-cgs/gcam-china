# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L225.hydrogen_china
#'
#' Selects the subsectors to be removed from the hydrogen sectors in GCAM-China
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L225.DeleteSubsector_h2_CHINA}. The corresponding file in the
#' original data system was \code{L225.hydrogen_CHINA.R} (gcam-china level2).
#' @details This chunk selects the subsectors to be removed from the hydrogen sectors in GCAM-China at the national level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YangLiu September 2018 / YangOu July 2023
module_gcamchina_L225.hydrogen_china <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             "L225.Supplysector_h2",
             "L225.SectorUseTrialMarket_h2",
             "L225.SubsectorLogit_h2",
             "L225.SubsectorShrwtFllt_h2",
             "L225.StubTech_h2",
             "L225.GlobalTechCoef_h2"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L225.DeleteSupplysector_h2_CHINA",
             "L225.Supplysector_h2_CHINA",
             "L225.SectorUseTrialMarket_h2_CHINA",
             "L225.SubsectorLogit_h2_CHINA",
             "L225.SubsectorShrwtFllt_h2_CHINA",
             "L225.StubTech_h2_CHINA",
             "L225.StubTechMarket_h2_CHINA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region <- subsector <- supplysector <- sector.name <- subsector.name <- technology <-
      state <- grid_region <- minicam.energy.input <- market.name <- stub.technology <- year <- NULL  # silence package check notes

    # Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = TRUE)
    L225.Supplysector_h2 <- get_data(all_data, "L225.Supplysector_h2", strip_attributes = TRUE)
    L225.SectorUseTrialMarket_h2 <- get_data(all_data, "L225.SectorUseTrialMarket_h2", strip_attributes = TRUE)
    L225.SubsectorLogit_h2 <- get_data(all_data, "L225.SubsectorLogit_h2", strip_attributes = TRUE)
    L225.SubsectorShrwtFllt_h2 <- get_data(all_data, "L225.SubsectorShrwtFllt_h2", strip_attributes = TRUE)
    L225.StubTech_h2 <- get_data(all_data, "L225.StubTech_h2", strip_attributes = TRUE)
    L225.GlobalTechCoef_h2 <- get_data(all_data, "L225.GlobalTechCoef_h2", strip_attributes = TRUE)

    # ===================================================
    # Delete the hydrogen sectors from the China region
    L225.DeleteSupplysector_h2_CHINA <- L225.Supplysector_h2 %>%
      filter(region == gcamchina.REGION) %>%
      select(LEVEL2_DATA_NAMES[["DeleteSupplysector"]])

    L225.Supplysector_h2_CHINA <- L225.Supplysector_h2 %>%
      filter(region == gcamchina.REGION) %>%
      write_to_all_provinces(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), gcamchina.PROVINCES_ALL)

    L225.SectorUseTrialMarket_h2_CHINA <- L225.SectorUseTrialMarket_h2 %>%
      filter(region == gcamchina.REGION) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["SectorUseTrialMarket"]], gcamchina.PROVINCES_ALL)

    L225.SubsectorLogit_h2_CHINA <- L225.SubsectorLogit_h2 %>%
      filter(region == gcamchina.REGION) %>%
      write_to_all_provinces(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), gcamchina.PROVINCES_ALL) %>%
      filter(!(region %in% c('HK', 'MC') & subsector %in% c('solar','wind')))

    L225.SubsectorShrwtFllt_h2_CHINA <- L225.SubsectorShrwtFllt_h2 %>%
      filter(region == gcamchina.REGION) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], gcamchina.PROVINCES_ALL) %>%
      filter(!(region %in% c('HK', 'MC') & subsector %in% c('solar','wind'))) %>%
      mutate(across(share.weight, as.numeric)) %>%
	    # change biomass share weight to 0.1
	    mutate(share.weight = if_else((subsector == "biomass")&(year.fillout == 2020),0.1,share.weight))


    L225.StubTech_h2_CHINA <- L225.StubTech_h2 %>%
      filter(region == gcamchina.REGION,
             !(region %in% c('HK', 'MC') & subsector %in% c('solar','wind'))) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["StubTech"]], gcamchina.PROVINCES_ALL) %>%
      filter(!(region %in% c('HK', 'MC') & subsector %in% c('solar','wind')))

    # Assign the market names. Use the USA region as the default, then
    # - re-set grid-region fuel market
    # - re-set state-level fuel markets
    # - re-set upstream hydrogen commodity markets (hack - this replacement will need to be updated when inter-state hydrogen markets are represented)
    L225.StubTechMarket_h2_CHINA <- L225.GlobalTechCoef_h2 %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) %>%
      mutate(market.name = gcamchina.REGION) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["StubTechMarket"]], gcamchina.PROVINCES_ALL) %>%
      left_join_error_no_match(select(province_names_mappings, province, grid_region = grid.region),
                               by = c("region" = "province")) %>%
      filter(!grepl("water", minicam.energy.input)) %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamchina.ELECT_TD_SECTORS,
                                   region, market.name), # electricity td province level
             market.name = if_else(minicam.energy.input %in% gcamchina.REGIONAL_FUEL_MARKETS,
                                   region, market.name),
             market.name = if_else(minicam.energy.input %in% L225.Supplysector_h2_CHINA$supplysector,
                                   region, market.name),
             market.name = if_else(minicam.energy.input == "trn_freight_road",
                                   region, market.name)) %>%
      filter(!(region %in% c('HK', 'MC') & subsector %in% c('solar','wind')))


    # ===================================================

    # Produce outputs
    L225.DeleteSupplysector_h2_CHINA %>%
      add_title("Remove hydrogen sectors of China region for GCAM-China") %>%
      add_units("Unitless") %>%
      add_comments("There are no China hydrogen sectors in GCAM-China") %>%
      add_precursors("L225.Supplysector_h2") ->
      L225.DeleteSupplysector_h2_CHINA

    L225.Supplysector_h2_CHINA %>%
      add_title("Supplysector info for hydrogen sectors in GCAM-China") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.Supplysector_h2") ->
      L225.Supplysector_h2_CHINA

    L225.SectorUseTrialMarket_h2_CHINA %>%
      add_title("Supplysector trial market assignments for hydrogen sectors in GCAM-China") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.SectorUseTrialMarket_h2") ->
      L225.SectorUseTrialMarket_h2_CHINA

    L225.SubsectorLogit_h2_CHINA %>%
      add_title("Logit exponents for hydrogen subsectors in GCAM-China") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.SubsectorLogit_h2") ->
      L225.SubsectorLogit_h2_CHINA

    L225.SubsectorShrwtFllt_h2_CHINA %>%
      add_title("Subsector shareweight fillout for hydrogen subsectors in GCAM-China") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.SubsectorShrwtFllt_h2") ->
      L225.SubsectorShrwtFllt_h2_CHINA

    L225.StubTech_h2_CHINA %>%
      add_title("Stub technology pointers for hydrogen sectors in GCAM-China") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.StubTech_h2") ->
      L225.StubTech_h2_CHINA

    L225.StubTechMarket_h2_CHINA %>%
      add_title("Stub technology market names for inputs to hydrogen technologies in GCAM-China") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "L225.GlobalTechCoef_h2") ->
      L225.StubTechMarket_h2_CHINA


    return_data(L225.DeleteSupplysector_h2_CHINA,
                L225.Supplysector_h2_CHINA,
                L225.SectorUseTrialMarket_h2_CHINA,
                L225.SubsectorLogit_h2_CHINA,
                L225.SubsectorShrwtFllt_h2_CHINA,
                L225.StubTech_h2_CHINA,
                L225.StubTechMarket_h2_CHINA)
  } else {
    stop("Unknown command")
  }
}
