# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamdata_L262.dac_china
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for dac-related gcam-china inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L262.SectorLogitTables[[ curr_table ]]$data}, \code{L262.Supplysector_dac}, \code{L262.FinalEnergyKeyword_dac},
#' \code{L262.SubsectorLogitTables[[ curr_table ]]$data}, \code{L262.SubsectorLogit_dac}, \code{L262.SubsectorShrwtFllt_dac},
#' \code{L262.SubsectorInterp_dac}, \code{L262.StubTech_dac}, \code{L262.GlobalTechShrwt_dac}, \code{L262.GlobalTechCoef_dac},
#' \code{L262.GlobalTechCost_dac}, \code{L262.GlobalTechCapture_dac}, \code{L262.StubTechProd_dac}, \code{L262.StubTechCalInput_dac_heat},
#' \code{L262.StubTechCoef_dac}, \code{L262.PerCapitaBased_dac}, \code{L262.BaseService_dac}, \code{L262.PriceElasticity_dac}, \code{object}.
#' The corresponding file in the original data system was \code{L262.dac.R} (energy level2).
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for dac sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr complete nesting
#' @author JF March 2021 / YO March 2022
module_gcamdata_L262.dac_china <- function(command, ...) {

   if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "gcam-china/CCS_provincial_data",
             FILE = "energy/calibrated_techs_cdr",
             FILE = "energy/A62.demand",
             "L262.CarbonCoef_dac",
             "L262.GlobalTechCoef_dac",
             "L262.Supplysector_dac",
             "L262.FinalEnergyKeyword_dac",
             "L262.SubsectorLogit_dac",
             "L262.SubsectorShrwtFllt_dac",
             "L262.SubsectorInterp_dac",
             "L262.StubTech_dac",
             "L262.PerCapitaBased_dac",
             "L262.PriceElasticity_dac",
             "L262.StubTechProd_dac",
             "L262.GlobalTechShrwt_dac"))

   } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L262.DeleteSupplysector_chinadac",
             "L262.DeleteFinalDemand_chinadac",
             "L262.StubTechCoef_dac_china_ssp1",
             "L262.StubTechCoef_dac_china_ssp2",
             "L262.StubTechCoef_dac_china_ssp3",
             "L262.StubTechCoef_dac_china_ssp4",
             "L262.StubTechCoef_dac_china_ssp5",
             "L262.SubsectorLogit_dac_china",
             "L262.SubsectorShrwtFllt_dac_china",
             "L262.SubsectorInterp_dac_china",
             "L262.StubTech_dac_china",
             "L262.PerCapitaBased_dac_china",
             "L262.PriceElasticity_dac_china",
             "L262.FinalEnergyKeyword_dac_china",
             "L262.BaseService_dac_china",
             "L262.Supplysector_dac_china",
             "L262.StubTechProd_dac_china",
             "L262.CarbonCoef_dac_china",
             "L262.StubTechShrwt_dac_china_ssp1",
             "L262.StubTechShrwt_dac_china_ssp2",
             "L262.StubTechShrwt_dac_china_ssp3",
             "L262.StubTechShrwt_dac_china_ssp4",
             "L262.StubTechShrwt_dac_china_ssp5"))
   } else if(command == driver.MAKE) {

     all_data <- list(...)[[1]]

     # Silence package notes
     province <- region <- supplysector <- energy.final.demand <- region <- year <-
       value <- calibration <- sector <- subsector <- technology <- calOutputValue <-
       subs.share.weight <- share.weight.year <- fuel <- minicam.energy.input <-
       coefficient <- market.name <- grid_region <- stub.technology <- calibrated.value <-
       tech.share.weight <- object <- NULL

    # Load required inputs
    province_subregions <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = T)
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs_cdr", strip_attributes = T)
    CCS_provincial_data <- get_data(all_data, "gcam-china/CCS_provincial_data", strip_attributes = T)
    A62.demand <- get_data(all_data, "energy/A62.demand", strip_attributes = T)

    L262.GlobalTechCoef_dac <- get_data(all_data, "L262.GlobalTechCoef_dac", strip_attributes = T)
    L262.Supplysector_dac <- get_data(all_data, "L262.Supplysector_dac", strip_attributes = T)
    L262.FinalEnergyKeyword_dac <- get_data(all_data, "L262.FinalEnergyKeyword_dac", strip_attributes = T)
    L262.SubsectorLogit_dac <- get_data(all_data, "L262.SubsectorLogit_dac", strip_attributes = T)
    L262.SubsectorShrwtFllt_dac <- get_data(all_data, "L262.SubsectorShrwtFllt_dac", strip_attributes = T)
    L262.SubsectorInterp_dac <- get_data(all_data, "L262.SubsectorInterp_dac", strip_attributes = T)
    L262.StubTech_dac <- get_data(all_data, "L262.StubTech_dac", strip_attributes = T)
    L262.PerCapitaBased_dac <- get_data(all_data, "L262.PerCapitaBased_dac", strip_attributes = T)
    L262.PriceElasticity_dac <- get_data(all_data, "L262.PriceElasticity_dac", strip_attributes = T)
    L262.StubTechProd_dac <- get_data(all_data, "L262.StubTechProd_dac", strip_attributes = T)
    L262.CarbonCoef_dac <- get_data(all_data, "L262.CarbonCoef_dac", strip_attributes = T)
    L262.GlobalTechShrwt_dac <- get_data(all_data, "L262.GlobalTechShrwt_dac", strip_attributes = T)


    # ===================================================
    # 1. Perform computations

    # 1a. Supplysector information
    # L262.Supplysector_dac: Supply sector information for CO2 removal sector containing dac

    L262.Supplysector_dac %>%
      filter(region == gcamchina.REGION) %>%
      select(region, supplysector) ->
      L262.DeleteSupplysector_chinadac


    L262.PerCapitaBased_dac %>%
      filter(region == gcamchina.REGION) %>%
      select(region, energy.final.demand) ->
      L262.DeleteFinalDemand_chinadac


    dac_china_processing <- function(data, gcamchina.PROVINCES_NOHKMC) {

      # Subset the input data frame for the china region. The subsetted data will be used
      # to check to see if the data frame needs to be processed, it's assumed that if the china
      # is not found in the region column that regions have already been processed.

      check_df <- filter(data, region == 'China')

      if(nrow(check_df) == 0) {

        new_data <- data

      } else {

        # If the input data frame contains china region information
        # then expand the input data to all provinces.

        data %>%
          filter(region == "China") %>%
          write_to_all_provinces(names = names(data), provinces = gcamchina.PROVINCES_NOHKMC) %>%
          filter(region %in% gcamchina.PROVINCES_NOHKMC) ->
          new_data

      }

      return(new_data)
    } # end of function

    # Use the dac_china_processing function to check and or process the following data frames so that
    # all of the output data frames contain information for all provinces.
    L262.Supplysector_dac_china <- dac_china_processing(L262.Supplysector_dac, gcamchina.PROVINCES_NOHKMC)
    L262.FinalEnergyKeyword_dac_china <- dac_china_processing(L262.FinalEnergyKeyword_dac, gcamchina.PROVINCES_NOHKMC)
    L262.SubsectorLogit_dac_china <- dac_china_processing(L262.SubsectorLogit_dac, gcamchina.PROVINCES_NOHKMC)
    L262.SubsectorShrwtFllt_dac_china <- dac_china_processing(L262.SubsectorShrwtFllt_dac, gcamchina.PROVINCES_NOHKMC)
    L262.SubsectorInterp_dac_china <- dac_china_processing(L262.SubsectorInterp_dac, gcamchina.PROVINCES_NOHKMC)
    L262.StubTech_dac_china <- dac_china_processing(L262.StubTech_dac, gcamchina.PROVINCES_NOHKMC)
    L262.PerCapitaBased_dac_china <- dac_china_processing(L262.PerCapitaBased_dac, gcamchina.PROVINCES_NOHKMC)
    L262.PriceElasticity_dac_china <- dac_china_processing(L262.PriceElasticity_dac, gcamchina.PROVINCES_NOHKMC)
    L262.CarbonCoef_dac_china <- dac_china_processing(L262.CarbonCoef_dac, gcamchina.PROVINCES_NOHKMC)

    L262.GlobalTechCoef_dac_china <- dac_china_processing(L262.GlobalTechCoef_dac %>%
                                                        mutate(region = gcamchina.REGION), gcamchina.PROVINCES_NOHKMC)

    # calibrated DAC availability based on cumulative carbon storage
    cumStorage <- CCS_provincial_data %>%
      group_by(province.name) %>%
      summarise(cumStorage = sum(CO2_Mt)) %>%
      ungroup() %>%
      mutate(region = gcamchina.REGION) %>%
      group_by(region) %>%
      mutate(cumStorage.share = cumStorage/sum(cumStorage)) %>%
      ungroup() %>%
      select(region, province.name, cumStorage.share)

    # downscale china total DAC calibration to province by cumulative C storage share
    L262.StubTechProd_dac %>%
      select(region, sector = supplysector, year, calOutputValue) %>%
      filter(region == gcamchina.REGION) %>%
      # map to all provinces, no. of rows will change
      left_join(cumStorage, by = "region") %>%
      mutate(value = cumStorage.share * calOutputValue) %>%
      select(province.name, sector, year, value) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = signif(value, energy.DIGITS_CALOUTPUT),
             region = province.name) %>%
      left_join(province_subregions,by=c('region'='province.name')) %>%
      mutate(region = province) ->
      L262.StubTechProd_dac_china

    # Subset the calibrated intermediate sectors and fuels to supplysector / subsector / technology
    # mapping file for unique sector / calibration / supplysector/ subsector / technology combinations.
    # This tibble will be used to add dac sector information add to the province dac
    # input table.
    calibrated_techs %>%
      # We are only interested in the technology IDs where calibration = output.
      filter(calibration == "output") %>%
      select(sector, calibration, supplysector, subsector, technology) %>%
      distinct ->
      calibrated_techs_dac_sector_info

    # Combine the dac sector information found above and the stub-technology calibrated
    # dac production into a single data frame.
    L262.StubTechProd_dac_china %>%
      left_join_error_no_match(calibrated_techs_dac_sector_info, by = "sector") %>%
      select(province, sector, calOutputValue, year, region, supplysector, subsector, technology) ->
      L262.StubTechProd_dac_china

    # Add share weight information to the province dac production data frame and format.
    L262.StubTechProd_dac_china %>%
      mutate(stub.technology = technology,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(region, supplysector, subsector, stub.technology, year, calOutputValue,
             share.weight.year, subs.share.weight, tech.share.weight) ->
      L262.StubTechProd_dac_china


    # Create the coefficients of dac production technologies input table.
    #
    # Start by creating a data frame of unique sector, fuel, supplysector, subsector,
    # technology, and minicam.energy.input combinations. This data frame will be used to
    # add sector information to input-ouput coefficients for province dac production.
    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct ->
      dac_production_technologies


     L262.GlobalTechCoef_dac_china %>%
     dplyr::rename(supplysector = sector.name,
             subsector = subsector.name,
             value = coefficient) %>%
      # Add market information. process heat are province level markets where as electricity
      # comes from the grid level.
      mutate(market.name = region,
      market.name = if_else(grepl("elec", minicam.energy.input), gcamchina.REGION, market.name)) %>%
      # replace market name with the grid region name if the minicam.energy.input is
      # considered a regional fuel market
      left_join(province_subregions %>%
                                 select(province, grid.region), by = c("region"="province")) %>%
      select(-grid.region) %>%
      # Change market name to reflect the fact that electricity is consumed from province markets.
      mutate(market.name = if_else(minicam.energy.input %in% gcamchina.ELECT_TD_SECTORS,
                                   region, market.name)) %>%
      dplyr::rename(stub.technology = technology,
            coefficient = value) ->
      L262.StubTechCoef_dac_china


    # Create the base-year service output for dac final demand input table.
    #
    # Since base service is equal to the output of the dac supplysector use
    # the coefficients from the stub technology production data frame and add
    # energy.final.demand form dac final demand perCapitaBased and price elasticity
    # assumption file.

    L262.StubTechProd_dac_china %>%
      mutate(energy.final.demand = A62.demand$energy.final.demand) %>%
      select(region, energy.final.demand, year, base.service = calOutputValue) ->
      L262.BaseService_dac_china

    L262.StubTechShrwt_dac_china <- L262.StubTechCoef_dac_china %>%
      left_join(L262.GlobalTechShrwt_dac,by=c("supplysector"="sector.name","subsector"="subsector.name","stub.technology"="technology","scenario","year")) %>%
      distinct(supplysector,subsector,stub.technology,year,region,share.weight,scenario) %>%
      select(supplysector,subsector,stub.technology,year,region,share.weight,scenario) %>%
      mutate(share.weight = if_else(stub.technology == 'hightemp DAC NG',0,share.weight))
    # ===================================================
    # Produce outputs

    L262.DeleteSupplysector_chinadac %>%
      add_title("Supply sector information for CO2 removal sector") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A62.sector is expended into all GCAM regions") %>%
      add_legacy_name("L262.DeleteSupplysector_chinadac") %>%
      add_precursors("L262.Supplysector_dac") ->
      L262.DeleteSupplysector_chinadac

    L262.DeleteFinalDemand_chinadac %>%
      add_title("Supply sector keywords for dac sector") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the supply sector final energy keywords from A62.sector are expended into all GCAM regions") %>%
      add_legacy_name("L262.DeleteFinalDemand_chinadac") %>%
      add_precursors("L262.FinalEnergyKeyword_dac") ->
      L262.DeleteFinalDemand_chinadac

    L262.SubsectorLogit_dac_china %>%
      add_title("Supply sector keywords for dac sector in all provinces") %>%
      add_units("Unitless") %>%
      add_comments("Expanded subsector logit exponents to to prospective dac provinces in region china") %>%
      add_legacy_name("L262.SubsectorLogit_dac_china") %>%
      add_precursors("L262.SubsectorLogit_dac") ->
      L262.SubsectorLogit_dac_china

    L262.SubsectorShrwtFllt_dac_china %>%
      add_title("Subsector shareweights of dac sector in dac producing provinces") %>%
      add_units("Unitless") %>%
      add_comments("Expanded subsector shareweights to to all provinces in region china") %>%
      add_legacy_name("L262.SubsectorShrwtFllt_dac_china") %>%
      add_precursors("L262.SubsectorShrwtFllt_dac") ->
      L262.SubsectorShrwtFllt_dac_china

    L262.SubsectorInterp_dac_china %>%
      add_title("Subsector shareweight interpolation of dac sector in all provinces") %>%
      add_units("NA") %>%
      add_comments("Expanded subsector shareweight interpolation to all provinces in region china") %>%
      add_legacy_name("L262.SubsectorInterp_dac_china") %>%
      add_precursors("L262.SubsectorInterp_dac") ->
      L262.SubsectorInterp_dac_china

    L262.StubTech_dac_china %>%
      add_title("Identification of stub technologies of dac in all provinces") %>%
      add_units("NA") %>%
      add_comments("Expanded identification of stub technologies of dac to all provinces in region china") %>%
      add_legacy_name("L262.StubTech_dac_china") %>%
      add_precursors("L262.StubTech_dac") ->
      L262.StubTech_dac_china

    L262.PerCapitaBased_dac_china %>%
      add_title("Per-capita based flag for dac exports final demand in all provinces") %>%
      add_units("NA") %>%
      add_comments("Expanded Per-capita based flag for dac exports final demand to all provinces in region china") %>%
      add_legacy_name("L262.PerCapitaBased_dac_china") %>%
      add_precursors("L262.PerCapitaBased_dac") ->
      L262.PerCapitaBased_dac_china

    L262.PriceElasticity_dac_china %>%
      add_title("Price elasticity for dac in all provinces") %>%
      add_units("Unitless") %>%
      add_comments("Expanded price elasticity for dac to all provinces in region china") %>%
      add_legacy_name("L262.PriceElasticity_dac_china") %>%
      add_precursors("L262.PriceElasticity_dac") ->
      L262.PriceElasticity_dac_china

    L262.FinalEnergyKeyword_dac_china %>%
      add_title("Subsector logit exponents of dac sector in dac producing provinces") %>%
      add_units("Unitless") %>%
      add_comments("Expanded supply sector keywords information dac producing provinces in region china") %>%
      add_legacy_name("L262.FinalEnergyKeyword_dac_china") %>%
      add_precursors("L262.FinalEnergyKeyword_dac") ->
      L262.FinalEnergyKeyword_dac_china

    # create L262.StubTechCoef_dac_china for five ssps
    TECH_PARAMETRIZATION_OUTPUTS <- paste0("ssp", 1:5)

    for(sce in TECH_PARAMETRIZATION_OUTPUTS) {
      L262.StubTechCoef_dac_china %>%
        filter(scenario == sce) %>%
        select(-c(scenario))%>%
        add_title(paste("dac technologies by province -", sce)) %>%
        add_units("coefficient = GJ/kg (gigajoules per kilogram of dac)") %>%
        add_comments("Rename markets with regional gird name if using regional regional fuel markets") %>%
        add_legacy_name("L262.StubTechCoef_dac_china") %>%
        add_precursors("L262.GlobalTechCoef_dac","gcam-china/province_names_mappings") ->
        x
      assign(paste0("L262.StubTechCoef_dac_china_", tolower(sce)), x)
    }

    for(sce in TECH_PARAMETRIZATION_OUTPUTS) {
      L262.StubTechShrwt_dac_china %>%
        filter(scenario == sce) %>%
        select(-c(scenario))%>%
        add_title(paste("dac technologies shareweight by province -", sce)) %>%
        add_units("unitless") %>%
        add_comments("disable natural gas DAC in china") %>%
        add_precursors("L262.GlobalTechShrwt_dac") ->
        x
      assign(paste0("L262.StubTechShrwt_dac_china_", tolower(sce)), x)
    }

    L262.Supplysector_dac_china %>%
      add_title("Supply sector information for dac sector in all provinces") %>%
      add_units("NA") %>%
      add_comments("Expanded supply sector information to dac producing provinces in region china") %>%
      add_legacy_name("L262.Supplysector_dac_china") %>%
      add_precursors("L262.Supplysector_dac") ->
      L262.Supplysector_dac_china

    L262.StubTechProd_dac_china %>%
      add_title("Dac calibration coefficients to all provinces") %>%
      add_units("NA") %>%
      add_comments("Added dac calibration coefficients to all provinces") %>%
      add_legacy_name("L262.StubTechProd_dac_china") %>%
      add_precursors("gcam-china/CCS_provincial_data","energy/A62.demand",
                    "energy/calibrated_techs_cdr", "L262.StubTechProd_dac") ->
      L262.StubTechProd_dac_china

    L262.BaseService_dac_china %>%
      add_title("Base-year service output for dac final demand input table") %>%
      add_units("NA") %>%
      add_comments("base service is equal to the output of the dac supplysector use") %>%
      add_legacy_name("L262.BaseService_dac_china") %>%
      same_precursors_as("L262.StubTechProd_dac_china") ->
      L262.BaseService_dac_china

    L262.CarbonCoef_dac_china %>%
      add_title("Carbon coefficent for airCO2 defined at province level") %>%
      add_units("NA") %>%
      add_comments("copy the same value 1 to all provinces") %>%
      add_legacy_name("L262.CarbonCoef_dac_china") %>%
      add_precursors("L262.CarbonCoef_dac") ->
      L262.CarbonCoef_dac_china


    return_data(L262.DeleteSupplysector_chinadac,
                L262.DeleteFinalDemand_chinadac,
                L262.SubsectorLogit_dac_china,
                L262.SubsectorShrwtFllt_dac_china,
                L262.SubsectorInterp_dac_china,
                L262.StubTech_dac_china,
                L262.PerCapitaBased_dac_china,
                L262.PriceElasticity_dac_china,
                L262.FinalEnergyKeyword_dac_china,
                L262.BaseService_dac_china,
                L262.StubTechCoef_dac_china_ssp1,
                L262.StubTechCoef_dac_china_ssp2,
                L262.StubTechCoef_dac_china_ssp3,
                L262.StubTechCoef_dac_china_ssp4,
                L262.StubTechCoef_dac_china_ssp5,
                L262.Supplysector_dac_china,
                L262.StubTechProd_dac_china,
                L262.CarbonCoef_dac_china,
                L262.StubTechShrwt_dac_china_ssp1,
                L262.StubTechShrwt_dac_china_ssp2,
                L262.StubTechShrwt_dac_china_ssp3,
                L262.StubTechShrwt_dac_china_ssp4,
                L262.StubTechShrwt_dac_china_ssp5)
  } else {
    stop("Unknown command")
  }
}
