# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L225.hydrogen
#'
#' Selects the subsectors to be removed from the hydrogen sectors in gcam-china
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L225.DeleteSubsector_h2_CHINA}. The corresponding file in the
#' original data system was \code{L225.hydrogen_CHINA.R} (gcam-china level2).
#' @details This chunk selects the subsectors to be removed from the hydrogen sectors in gcam-china at the national level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YangLiu September 2018 / YangOu July 2023
module_gcamchina_L225.hydrogen <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             "L225.Supplysector_h2",
             "L225.SectorUseTrialMarket_h2",
             "L225.SubsectorLogit_h2",
             "L225.SubsectorShrwtFllt_h2",
             "L225.StubTech_h2",
             "L225.GlobalTechCoef_h2",
             "L201.Pop_GCAMCHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L225.DeleteSupplysector_h2_CHINA",
             "L225.Supplysector_h2_CHINA",
             "L225.SectorUseTrialMarket_h2_CHINA",
             "L225.SubsectorLogit_h2_CHINA",
             "L225.SubsectorShrwtFllt_h2_CHINA",
             "L225.StubTech_h2_CHINA",
             "L225.StubTechMarket_h2_CHINA",
             "L225.DeleteStubTechMinicamEnergyInput_H2_CHINA",
             "L225.Supplysector_h2_ind_CHINA",
             "L225.SubsectorLogit_h2_ind_CHINA",
             "L225.SubsectorShrwtFllt_h2_ind_CHINA",
             "L225.TechCoef_h2_ind_CHINA",
             "L225.TechShrwt_h2_ind_CHINA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region <- subsector <- supplysector <- sector.name <- subsector.name <- technology <-
      province <- grid_region <- minicam.energy.input <- market.name <- stub.technology <- year <- NULL  # silence package check notes

    # Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = TRUE)
    L225.Supplysector_h2 <- get_data(all_data, "L225.Supplysector_h2", strip_attributes = TRUE)
    L225.SectorUseTrialMarket_h2 <- get_data(all_data, "L225.SectorUseTrialMarket_h2", strip_attributes = TRUE)
    L225.SubsectorLogit_h2 <- get_data(all_data, "L225.SubsectorLogit_h2", strip_attributes = TRUE)
    L225.SubsectorShrwtFllt_h2 <- get_data(all_data, "L225.SubsectorShrwtFllt_h2", strip_attributes = TRUE)
    L225.StubTech_h2 <- get_data(all_data, "L225.StubTech_h2", strip_attributes = TRUE)
    L225.GlobalTechCoef_h2 <- get_data(all_data, "L225.GlobalTechCoef_h2", strip_attributes = TRUE)

    L201.Pop_GCAMCHINA <- get_data(all_data, "L201.Pop_GCAMCHINA", strip_attributes = TRUE)

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

    # Assign the market names. Use the China region as the default, then
    # - re-set grid-region fuel market
    # - re-set province-level fuel markets
    # - re-set upstream hydrogen commodity markets (hack - this replacement will need to be updated when inter-province hydrogen markets are represented)
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


    L225.StubTechMarket_h2_CHINA %>%
      filter(minicam.energy.input == "PV_resource") %>%
      mutate(minicam.energy.input = "global solar resource") %>%
      select(region, supplysector, subsector, stub.technology, year, minicam.energy.input) ->
      L225.DeleteStubTechMinicamEnergyInput_H2_CHINA

    # create "H2 industrial" supplysector in China region with subsectors/technologies for each province
    L225.Supplysector_h2_ind_CHINA <- L225.Supplysector_h2_CHINA %>%
      filter(supplysector == 'H2 industrial') %>%
      mutate(region = gcamchina.REGION)

    L225.SubsectorLogit_h2_ind_CHINA <- L225.SubsectorLogit_h2_CHINA %>%
      filter(supplysector == 'H2 industrial') %>%
      distinct(region,.keep_all=TRUE) %>%
      mutate(subsector = paste0(region,' ',supplysector),
             region = gcamchina.REGION)

    L225.PopShrwts <- L201.Pop_GCAMCHINA %>%
      group_by(year) %>%
      mutate(popShrwt = totalPop / sum(totalPop)) %>%
      ungroup()

    # These share-weights are revised each model time period, according to the population share over time.
    # Full_join is used as an expanding join is wanted here (expanding by year)
    L225.SubsectorShrwtFllt_h2_ind_CHINA <- L225.SubsectorShrwtFllt_h2_CHINA %>%
      filter(supplysector == 'H2 industrial') %>%
      distinct(region,year.fillout,.keep_all=TRUE) %>%
      full_join(L225.PopShrwts, by = c('region')) %>%
      mutate(subsector = paste0(region,' ',supplysector),
             region = gcamchina.REGION,
             share.weight = if_else(as.numeric(share.weight) != 0, round(popShrwt,energy.DIGITS_SHRWT), as.numeric(share.weight)))

    # Full_join is used here in order to expand a global technology table by region (province)
    L225.TechCoef_h2_ind_CHINA <- L225.GlobalTechCoef_h2 %>%
      filter(sector.name == 'H2 industrial') %>%
      full_join(province_names_mappings %>%
                  select(province) %>%
                  mutate(sector.name = 'H2 industrial'),by = c('sector.name')) %>%
      distinct(province,year,.keep_all=TRUE) %>%
      mutate(subsector.name = paste0(province,' ',sector.name),
             technology = paste0(province,' ',sector.name),
             minicam.energy.input = 'H2 industrial',
             region = gcamchina.REGION,
             market.name = province) %>%
      rename(supplysector = sector.name,
             subsector = subsector.name) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]])

    L225.TechShrwt_h2_ind_CHINA <- L225.TechCoef_h2_ind_CHINA %>%
      mutate(share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]])

    # ===================================================

    # Produce outputs
    L225.DeleteSupplysector_h2_CHINA %>%
      add_title("Remove hydrogen sectors of China region for gcam-china") %>%
      add_units("Unitless") %>%
      add_comments("There are no China hydrogen sectors in gcam-china") %>%
      add_precursors("L225.Supplysector_h2") ->
      L225.DeleteSupplysector_h2_CHINA

    L225.Supplysector_h2_CHINA %>%
      add_title("Supplysector info for hydrogen sectors in gcam-china") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.Supplysector_h2") ->
      L225.Supplysector_h2_CHINA

    L225.SectorUseTrialMarket_h2_CHINA %>%
      add_title("Supplysector trial market assignments for hydrogen sectors in gcam-china") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.SectorUseTrialMarket_h2") ->
      L225.SectorUseTrialMarket_h2_CHINA

    L225.SubsectorLogit_h2_CHINA %>%
      add_title("Logit exponents for hydrogen subsectors in gcam-china") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.SubsectorLogit_h2") ->
      L225.SubsectorLogit_h2_CHINA

    L225.SubsectorShrwtFllt_h2_CHINA %>%
      add_title("Subsector shareweight fillout for hydrogen subsectors in gcam-china") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.SubsectorShrwtFllt_h2") ->
      L225.SubsectorShrwtFllt_h2_CHINA

    L225.StubTech_h2_CHINA %>%
      add_title("Stub technology pointers for hydrogen sectors in gcam-china") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.StubTech_h2") ->
      L225.StubTech_h2_CHINA

    L225.StubTechMarket_h2_CHINA %>%
      add_title("Stub technology market names for inputs to hydrogen technologies in gcam-china") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "L225.GlobalTechCoef_h2") ->
      L225.StubTechMarket_h2_CHINA

    L225.DeleteStubTechMinicamEnergyInput_H2_CHINA %>%
      add_title("Delete global solar resource Energy Input for PV Technologies") %>%
      add_units("NA") %>%
      add_comments("global solar resource input deleted; will be replaced by PV_resource") %>%
      add_comments("Applies to all states") %>%
      add_legacy_name("L225.DeleteStubTechMinicamEnergyInput_H2_CHINA") %>%
      add_precursors('L225.Supplysector_h2') ->
      L225.DeleteStubTechMinicamEnergyInput_H2_CHINA

    L225.Supplysector_h2_ind_CHINA %>%
      add_title("Add back H2 industrial to China region") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.Supplysector_h2") ->
      L225.Supplysector_h2_ind_CHINA

    L225.SubsectorLogit_h2_ind_CHINA %>%
      add_title("province-level logit exponents for H2 industrial in gcam-china") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("L225.SubsectorLogit_h2") ->
      L225.SubsectorLogit_h2_ind_CHINA

    L225.SubsectorShrwtFllt_h2_ind_CHINA %>%
      add_title("Subsector shareweight fillout for province-level H2 industrial in gcam-china") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights based on relative population in each province") %>%
      add_precursors("L225.SubsectorShrwtFllt_h2",
                     "L201.Pop_GCAMCHINA") ->
      L225.SubsectorShrwtFllt_h2_ind_CHINA

    L225.TechCoef_h2_ind_CHINA %>%
      add_title("Technology market names for inputs to province-level H2 industrial technologies in gcam-china") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "L225.GlobalTechCoef_h2") ->
      L225.TechCoef_h2_ind_CHINA

    L225.TechShrwt_h2_ind_CHINA %>%
      add_title("Technology market names for inputs to province-level H2 industrial technologies in gcam-china") %>%
      add_units("Unitless") %>%
      add_comments("Mirror of information in all regions") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "L225.GlobalTechCoef_h2") ->
      L225.TechShrwt_h2_ind_CHINA

    return_data(L225.DeleteSupplysector_h2_CHINA,
                L225.Supplysector_h2_CHINA,
                L225.SectorUseTrialMarket_h2_CHINA,
                L225.SubsectorLogit_h2_CHINA,
                L225.SubsectorShrwtFllt_h2_CHINA,
                L225.StubTech_h2_CHINA,
                L225.StubTechMarket_h2_CHINA,
                L225.DeleteStubTechMinicamEnergyInput_H2_CHINA,
                L225.Supplysector_h2_ind_CHINA,
                L225.SubsectorLogit_h2_ind_CHINA,
                L225.SubsectorShrwtFllt_h2_ind_CHINA,
                L225.TechCoef_h2_ind_CHINA,
                L225.TechShrwt_h2_ind_CHINA)
  } else {
    stop("Unknown command")
  }
}
