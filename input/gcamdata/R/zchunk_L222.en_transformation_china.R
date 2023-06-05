#' module_gcam.china_L222.en_transformation_china
#'
#' Prepare the assumptions and calibrated outputs for energy transformation supplysectors, subsectors, and technologies specific to China sectors and/or provinces.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L222.DeleteStubTech_CHINAen}, \code{L222.PassThroughSector_CHINAen}, \code{L222.Tech_CHINAen}, \code{L222.TechShrwt_CHINAen}, \code{L222.TechInterp_CHINAen}, \code{L222.TechShrwt_CHINAen}, \code{L222.TechCoef_CHINAen}, \code{L222.Production_CHINArefining}, \code{L222.SectorLogitTables_CHINA[[ curr_table ]]$data}, \code{L222.Supplysector_en_CHINA}, \code{L222.SubsectorShrwtFllt_en_CHINA}, \code{L222.StubTechProd_refining_CHINA}, \code{L222.StubTechMarket_en_CHINA}, \code{L222.CarbonCoef_en_CHINA}. The corresponding file in the
#' original data system was \code{L222.en_transformation_CHINA.R} (gcam-china level2).
#' @details This chunk sets up the USA energy transformation technology databases as well as writing out assumptions to all states/sectors/markets for shareweights and logits.
#' Calibrated outputs and I:O coefficients are updated from global values produced by \code{\link{module_energy_L222.en_transformation}}.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join mutate one_of pull select summarise
#' @importFrom tidyr gather spread
#' @author BY Jun 2019

module_gcam.china_L222.en_transformation_china <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L222.StubTech_en",
             "L122.out_EJ_province_refining_F",
             "L222.SubsectorLogit_en",
             FILE="energy/calibrated_techs",
             "L222.StubTechCoef_refining",
             "L222.GlobalTechInterp_en",
             "L222.GlobalTechCoef_en",
             "L222.GlobalTechCost_en",
             "L222.GlobalTechShrwt_en",
             "L222.GlobalTechCapture_en",
             # "L222.GlobalTechShutdownProfit_en",
             # "L222.GlobalTechShutdown_en",
             # "L222.GlobalTechSCurveProfit_en",
             "L222.GlobalTechSCurve_en",
             # "L222.GlobalTechLifetimeProfit_en",
             # "L222.GlobalTechLifetime_en",
             "L222.Supplysector_en",
             "L202.CarbonCoef"
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L222.DeleteStubTech_CHINAen", "L222.PassThroughSector_CHINAen", "L222.Tech_CHINAen",
             "L222.TechShrwt_CHINAen", "L222.TechInterp_CHINAen", "L222.TechCoef_CHINAen", "L222.Production_CHINArefining",
             "L222.Supplysector_en_CHINA", "L222.SubsectorShrwtFllt_en_CHINA", "L222.StubTechProd_refining_CHINA", "L222.StubTechMarket_en_CHINA",
             "L222.CarbonCoef_en_CHINA", "L222.GlobalTechSCurve_en_CHINA",
             "L222.GlobalTechCost_en_CHINA",
             "L222.SubsectorLogit_en_CHINA",
             "L222.StubTech_en_CHINA",
             "L222.StubTechCoef_refining_CHINA",
             "L222.GlobalTechInterp_en_CHINA",
             "L222.GlobalTechCoef_en_CHINA",
             "L222.GlobalTechShrwt_en_CHINA",
             "L222.GlobalTechCapture_en_CHINA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L222.StubTech_en <- get_data(all_data, "L222.StubTech_en", strip_attributes = T)
    L122.out_EJ_province_refining_F <- get_data(all_data, "L122.out_EJ_province_refining_F", strip_attributes = T)
    L222.SubsectorLogit_en <- get_data(all_data, "L222.SubsectorLogit_en", strip_attributes = T)
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs", strip_attributes = T)
    L222.StubTechCoef_refining <- get_data(all_data, "L222.StubTechCoef_refining", strip_attributes = T)
    L222.GlobalTechInterp_en <- get_data(all_data, "L222.GlobalTechInterp_en", strip_attributes = T)
    L222.GlobalTechCoef_en <- get_data(all_data, "L222.GlobalTechCoef_en", strip_attributes = T)
    L222.GlobalTechCost_en <- get_data(all_data, "L222.GlobalTechCost_en", strip_attributes = T)
    L222.GlobalTechShrwt_en <- get_data(all_data, "L222.GlobalTechShrwt_en", strip_attributes = T)
    L222.GlobalTechCapture_en <- get_data(all_data, "L222.GlobalTechCapture_en", strip_attributes = T)
    # L222.GlobalTechShutdownProfit_en <- get_data(all_data, "L222.GlobalTechShutdownProfit_en")
    # L222.GlobalTechShutdown_en <- get_data(all_data, "L222.GlobalTechShutdown_en")
    # L222.GlobalTechSCurveProfit_en <- get_data(all_data, "L222.GlobalTechSCurveProfit_en")
    L222.GlobalTechSCurve_en <- get_data(all_data, "L222.GlobalTechSCurve_en", strip_attributes = T)
    # L222.GlobalTechLifetimeProfit_en <- get_data(all_data, "L222.GlobalTechLifetimeProfit_en")
    # L222.GlobalTechLifetime_en <- get_data(all_data, "L222.GlobalTechLifetime_en")
    L222.Supplysector_en <- get_data(all_data, "L222.Supplysector_en", strip_attributes = T)
    L202.CarbonCoef <- get_data(all_data, "L202.CarbonCoef", strip_attributes = T)

    # silence check package notes
    calOutputValue <- calibration <- minicam.energy.input <- province <- region <-
    sector <- sector.name <- subsector <- subsector.name <- supplysector <-
    supplysector.x <- supplysector.y <- technology <- value <- year <- market.name <- NULL

    # ===================================================
    # Define sector(s) used in L222.en_transformation_China
    # The supplysector and subsector structure in these sectors are retained
    gcamchina.SECTOR_EN_NAMES <- "refining"
    gcamchina.SECTOR_oil_refining <- "oil refining"
    gcamchina.SECTOR_bio_liquids <- "biomass liquids"
    gcamchina.SECTOR_coal_to_liquids <- "coal to liquids"
    gcamchina.SECTOR_gas_to_liquids <- "gas to liquids"



    # NOTE: Oil refining sectors are only created in provinces where the production is >0 in the historical period. Other techs are available everywhere
    L122.out_EJ_province_refining_F %>%
      filter(sector == gcamchina.SECTOR_oil_refining) %>%
      group_by(province) %>%
      summarize(value = sum(value)) %>%
      ungroup() %>%
      filter(value > 0) %>%
      select(province) ->
      oil_refining_provinces

    # ===================================================
    # Some helpful functions:
    #
    # global_energy_to_China_nonGlobalTech - takes global energy inputs for non global tech
    # from L222.en_transformation.R and processes for use in China
    global_energy_to_China_nonGlobalTech <- function(data) {
      data %>%
        filter(region == gcamchina.REGION,
               supplysector %in% gcamchina.SECTOR_EN_NAMES) %>%
        write_to_all_provinces(names = c(names(data), "region"), gcamchina.PROVINCES_NOHKMC) %>%
        filter((subsector == "oil refining" & region %in% oil_refining_provinces$province) |
                 subsector != "oil refining") %>%
        mutate(supplysector = subsector)
    } # global_energy_to_China_nonGlobalTech

    # global_energy_to_China_GlobalTech - takes global energy inputs for global tech
    # from L222.en_transformation.R and processes for use in USA
    global_energy_to_China_GlobalTech <- function(data) {
      data %>%
        filter(sector.name %in% gcamchina.SECTOR_EN_NAMES) %>%
        mutate(sector.name = subsector.name)
    } # global_energy_to_China_GlobalTech

    # ===================================================

    # L222.DeleteStubTech_CHINAen: remove existing stub technologies in the CHINA region
    # TThe supplysector and subsector structure in the sectors defined in gcamchina.SECTOR_EN_NAMES are retained
    L222.StubTech_en %>%
      filter(region == gcamchina.REGION,
             supplysector %in% gcamchina.SECTOR_EN_NAMES) ->
      L222.DeleteStubTech_CHINAen

    # L222.PassThroughSector_CHINAen: PassThroughSector information to send vintaging info from provinces to CHINA
   L222.Tech_CHINAen <- L222.SubsectorLogit_en %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      filter(region == gcamchina.REGION, supplysector %in% gcamchina.SECTOR_EN_NAMES) %>%
      repeat_add_columns(tibble::tibble(province = gcamchina.PROVINCES_NOHKMC) ) %>%
      filter(subsector == gcamchina.SECTOR_oil_refining & province %in% oil_refining_provinces$province
             | subsector != gcamchina.SECTOR_oil_refining) %>%
      mutate(technology = paste(province, subsector, sep = " "))

    # L222.Tech_CHINAen : The technology pass-throughs used to set the proper node name, CHINA region.
    L222.Tech_CHINAen %>%
      select(province, LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      rename(region = province,
             pass.through.sector = subsector,
             marginal.revenue.sector = supplysector,
             marginal.revenue.market = region) ->
      L222.PassThroughSector_CHINAen


    L222.Tech_CHINAen %>%
      select(LEVEL2_DATA_NAMES[["Tech"]]) ->
      L222.Tech_CHINAen

    # L222.TechInterp_CHINAen: technology shareweights, CHINA region
    # NOTE: Technology interpolation only applies to calibrated technologies
    L222.Tech_CHINAen %>%
      filter(subsector %in% c(gcamchina.SECTOR_oil_refining, gcamchina.SECTOR_bio_liquids,gcamchina.SECTOR_coal_to_liquids)) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_FUTURE_YEARS),
            # For biomass liquids, allow province shares to shift over time (future techs are different than present techs)
            interpolation.function = if_else(subsector == gcamchina.SECTOR_bio_liquids, "s-curve", "fixed")) ->
      L222.TechInterp_CHINAen

    # L222.TechShrwt_CHINAen: technology shareweights, CHINA region
    L222.Tech_CHINAen %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      # Default the base year shareweights to 0. This will be over-ridden in calibration
      mutate(share.weight = case_when(year %in% MODEL_BASE_YEARS ~ 0,
                                      year %in% MODEL_FUTURE_YEARS ~ 1)) ->
      L222.TechShrwt_CHINAen


    # L222.TechCoef_CHINAen: technology coefficients and market names, CHINA region
    L222.TechShrwt_CHINAen %>%
      mutate(minicam.energy.input = subsector,
             coefficient = 1,
             # market.name is the province name, extracting it from the technology
             market.name = substr(technology, 1, nchar(technology) - nchar(subsector) - 1)) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L222.TechCoef_CHINAen

    # L222.Production_CHINArefining: calibrated refinery production in CHINA (consuming output of provinces)
    L122.out_EJ_province_refining_F %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(value = round(value, digits = gcamchina.DIGITS_CALOUTPUT),
             region = gcamchina.REGION) %>%
      rename(calOutputValue = value) %>%
      left_join_keep_first_only(select(calibrated_techs, sector, supplysector, subsector), by = "sector") %>%
      mutate(technology = paste(province, subsector, sep = " "),
             minicam.energy.input = subsector) %>%
      filter(subsector == gcamchina.SECTOR_oil_refining & province %in% oil_refining_provinces$province
             | subsector != gcamchina.SECTOR_oil_refining) %>%
      group_by(region, supplysector, subsector, technology, year, minicam.energy.input) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup() %>%
      set_subsector_shrwt() %>%
      mutate(share.weight.year = year,
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      select(LEVEL2_DATA_NAMES[["Production"]]) ->
      L222.Production_CHINArefining

    # TODO: figure out a better strategy.  We need to have at least one technology be available in the final
    # calibration year so we can get a base cost for the absolute cost logit.  Having a share weight of zero
    # at the subsector is sufficient then to ensure we get no production in the calibration years
    L222.GlobalTechShrwt_en[L222.GlobalTechShrwt_en$technology == "coal to liquids" & L222.GlobalTechShrwt_en$year == MODEL_FINAL_BASE_YEAR, "share.weight"] <- 1.0
    L222.GlobalTechShrwt_en[L222.GlobalTechShrwt_en$technology == "gas to liquids" & L222.GlobalTechShrwt_en$year == MODEL_FINAL_BASE_YEAR, "share.weight"] <- 1.0

    # Process energy files from L222.en_transformation.R for use in China,
    # slightly differently processing for global tech vs not inputs
    L222.SubsectorLogit_en_CHINA      <- global_energy_to_China_nonGlobalTech(L222.SubsectorLogit_en) #has an extra column logit.type : absolute-cost-logit
    L222.StubTech_en_CHINA            <- global_energy_to_China_nonGlobalTech(L222.StubTech_en) #doesn't have data for oil refining in GZ, SX, XZ, and all data in HK, MC
    L222.StubTechCoef_refining_CHINA  <- global_energy_to_China_nonGlobalTech(L222.StubTechCoef_refining) #doesn't have oil refining data for GZ, SX, XZ, HK, MC
    L222.GlobalTechInterp_en_CHINA    <- global_energy_to_China_GlobalTech(L222.GlobalTechInterp_en)
    L222.GlobalTechCoef_en_CHINA      <- global_energy_to_China_GlobalTech(L222.GlobalTechCoef_en)
    L222.GlobalTechCost_en_CHINA      <- global_energy_to_China_GlobalTech(L222.GlobalTechCost_en)
    L222.GlobalTechShrwt_en_CHINA     <- global_energy_to_China_GlobalTech(L222.GlobalTechShrwt_en)
    L222.GlobalTechCapture_en_CHINA   <- global_energy_to_China_GlobalTech(L222.GlobalTechCapture_en)
    L222.GlobalTechSCurve_en_CHINA    <- global_energy_to_China_GlobalTech(L222.GlobalTechSCurve_en)

    ### The same processing for optional/currently NULL inputs
    # L222.GlobalTechShutdownProfit_en_CHINA  <- global_energy_to_China_GlobalTech(L222.GlobalTechShutdownProfit_en)
    # L222.GlobalTechShutdown_en_CHINA        <- global_energy_to_China_GlobalTech(L222.GlobalTechShutdown_en)
    # L222.GlobalTechSCurveProfit_en_CHINA    <- global_energy_to_China_GlobalTech(L222.GlobalTechSCurveProfit_en)
    # L222.GlobalTechLifetimeProfit_en_CHINA  <- global_energy_to_China_GlobalTech(L222.GlobalTechLifetimeProfit_en)
    # L222.GlobalTechLifetime_en_CHINA        <- global_energy_to_China_GlobalTech(L222.GlobalTechLifetime_en)



    # L222.Supplysector_en_CHINA: Supplysector information, replace name of supplysector with the subsector names
    L222.SubsectorLogit_en_CHINA %>%
      select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
      left_join_error_no_match(distinct(select(L222.SubsectorLogit_en, supplysector, subsector)),
                               by = "subsector") %>%
      rename(supplysector = supplysector.x,
             old_supplysector = supplysector.y) %>%
      left_join_error_no_match(distinct(select(L222.Supplysector_en, -region)),
                               by = c("old_supplysector" = "supplysector")) %>%
      select(one_of(LEVEL2_DATA_NAMES[["Supplysector"]])) %>%
    # Note there is no competition here so just use the default logit type
      mutate(logit.type = gcamchina.DEFAULT_LOGIT_TYPE) ->
      L222.Supplysector_en_CHINA

    # L222.SubsectorShrwtFllt_en_CHINA: Subsector shareweights, there is no competition here, so just fill out with 1s
    # (will be over-ridden by base year calibration where necessary)
    L222.SubsectorLogit_en_CHINA %>%
      select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
      mutate(year = min(MODEL_YEARS),
             share.weight = gcamchina.DEFAULT_SHAREWEIGHT) ->
      L222.SubsectorShrwtFllt_en_CHINA

    # L222.StubTechProd_refining_CHINA: calibrated fuel production by province
    #
    # Step 1, process the table of calibrated_techs to only include calibration=output and relevant columns
    calibrated_techs %>%
      filter(calibration == "output") %>%
      select(sector, supplysector, subsector, technology) %>%
      distinct ->
      calibrated_techs_tmp

    # Step 2, process L122.out_EJ_province_refining_F, joining the processed table of calibrated_techs from step 1,
    # to create L222.StubTechProd_refining_CHINA. Note the supplysector is the same as the subsector within the states.
    L122.out_EJ_province_refining_F %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(region = province,
             calOutputValue = value) %>%
      mutate(calOutputValue = round(calOutputValue, gcamchina.DIGITS_CALOUTPUT)) %>%
      left_join_error_no_match(calibrated_techs_tmp, by = "sector") %>%
      #The supplysector is the same as the subsector within the provinces
      mutate(supplysector = subsector,
             stub.technology = technology,
             share.weight.year = year) %>%
      set_subsector_shrwt() %>%
      mutate(tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(one_of(LEVEL2_DATA_NAMES[["StubTechProd"]])) %>%
      # Remove oil refining tech from provinces that do not have any
      filter((subsector == "oil refining" & region %in% oil_refining_provinces$province) |
               subsector != "oil refining") ->
      L222.StubTechProd_refining_CHINA

    # L222.StubTechMarket_en_CHINA: market names of inputs to province refining sectors
    L222.GlobalTechCoef_en_CHINA %>%
      select(one_of(LEVEL2_DATA_NAMES[["GlobalTechInput"]])) %>%
      write_to_all_provinces(names = c(LEVEL2_DATA_NAMES[["GlobalTechInput"]], "region"), gcamchina.PROVINCES_NOHKMC) %>%
      rename(supplysector = sector.name,
             subsector = subsector.name,
             stub.technology = technology) %>%
      # Finish L222.StubTechMarket_en_CHINA by Setting electricity to the province markets
      mutate(market.name = if_else(minicam.energy.input %in% gcamchina.ELECT_TD_SECTORS, region, gcamchina.REGION)) ->
      L222.StubTechMarket_en_CHINA

    #If designated, switch fuel market names to the regional markets
    if( gcamchina.USE_REGIONAL_FUEL_MARKETS ){
      L222.StubTechMarket_en_CHINA %>%
        mutate(market.name = if_else(minicam.energy.input %in% gcamchina.REGIONAL_FUEL_MARKETS, region, market.name)) ->
        L222.StubTechMarket_en_CHINA
    }

    L222.StubTechMarket_en_CHINA %>%
         filter((subsector == "oil refining" & region %in% oil_refining_provinces$province) |
                  subsector != "oil refining") ->
         L222.StubTechMarket_en_CHINA

    # L222.CarbonCoef_en_CHINA: energy carbon coefficients in China
    #
    # Step 1, process L202.CarbonCoef for joining
    L202.CarbonCoef %>%
      filter(region == gcamchina.REGION) %>%
      select(-region) %>%
      distinct ->
      L202.CarbonCoef_tmp

    # Step 2, create L222.CarbonCoef_en_CHINA by joining the table from step 1.
    L222.Supplysector_en_CHINA %>%
      select(region, supplysector) %>%
      distinct %>%
      left_join_error_no_match(distinct(select(L222.TechShrwt_CHINAen, subsector, supplysector)),
                               by = c("supplysector" = "subsector")) %>%
      left_join_error_no_match(L202.CarbonCoef_tmp, by =  c("supplysector.y" = "PrimaryFuelCO2Coef.name")) %>%
      select(-supplysector.y) %>%
      rename(PrimaryFuelCO2Coef.name = supplysector) ->
      L222.CarbonCoef_en_CHINA

    # ===================================================
    # Produce outputs
    L222.DeleteStubTech_CHINAen %>%
      mutate(region = region) %>%  # strip off attributes so we can re-write title, etc.
      add_title("Removes existing stub technologies in the China region") %>%
      add_units("NA") %>%
      add_comments("Removes existing stub technologies in the China region from L222.StubTech_en.") %>%
      add_comments("The supplysector and subsector structure in the sectors defined in gcamchina.SECTOR_EN_NAMES are retained")  %>%
      add_legacy_name("L222.DeleteStubTech_CHINAen") %>%
      add_precursors("L222.StubTech_en") ->
      L222.DeleteStubTech_CHINAen

    L222.PassThroughSector_CHINAen %>%
      add_title("PassThroughSector information to send vintaging info from provinces to China") %>%
      add_units("NA") %>%
      add_comments("state, subsector, supplysector, and region from L222.Tech_CHINAen is renamed.") %>%
      add_legacy_name("L222.PassThroughSector_CHINAen") %>%
      same_precursors_as(L222.Tech_CHINAen) ->
      L222.PassThroughSector_CHINAen

    L222.Tech_CHINAen %>%
      add_title("The technology pass-throughs used to set the proper node name, CHINA region.") %>%
      add_units("units") %>%
      add_comments("China supplysector and subsector information from L222.SubsectorLogit_en is") %>%
      add_comments("repeated for all China provinces and updated.") %>%
      add_legacy_name("L222.Tech_CHINAen") %>%
      add_precursors("L222.SubsectorLogit_en") ->
      L222.Tech_CHINAen

    L222.TechInterp_CHINAen %>%
      add_title("Technology shareweights, CHINA region") %>%
      add_units("NA") %>%
      add_comments("Technology interpolation only applies to calibrated technologies.For biomass liquids, ") %>%
      add_comments("allow state shares to shift over time since future techs are different than present techs.") %>%
      add_legacy_name("L222.TechInterp_CHINAen")  %>%
      same_precursors_as(L222.Tech_CHINAen) ->
      L222.TechInterp_CHINAen

    L222.TechShrwt_CHINAen %>%
      add_title("technology shareweights, CHINA region") %>%
      add_units("NA") %>%
      add_comments("L222.Tech_CHINAen is repeated for model base year and future years and shareweights of 0 and") %>%
      add_comments("1 are added for each, respectively. Overwritten in calibration.") %>%
      add_legacy_name("L222.TechShrwt_CHINAen") %>%
      same_precursors_as(L222.Tech_CHINAen) ->
      L222.TechShrwt_CHINAen

    L222.TechCoef_CHINAen %>%
      add_title("Technology coefficients and market names, CHINA region") %>%
      add_units("units") %>%
      add_comments("Data from L222.TechShrwt_CHINAen is renamed and filled out.") %>%
      add_legacy_name("L222.TechCoef_CHINAen") %>%
      same_precursors_as(L222.TechShrwt_CHINAen) ->
      L222.TechCoef_CHINAen

    L222.Production_CHINArefining %>%
      add_title("Calibrated refinery production in CHINA (consuming output of provinces)") %>%
      add_units("NA") %>%
      add_comments("L122.out_EJ_province_refining_F is aggregated to the supplysector/subsector/technology level.") %>%
      add_legacy_name("L222.Production_CHINArefining") %>%
      add_precursors("energy/calibrated_techs",
                     "L122.out_EJ_province_refining_F") ->
      L222.Production_CHINArefining

    L222.SubsectorLogit_en_CHINA %>%
      add_title("Subsector logit competition info for China energy provinces and sectors") %>%
      add_units("NA") %>%
      add_comments("Subsector logit data from L222.SubsectorLogit_en are filtered and repeated") %>%
      add_comments("for China sectors in each province.") %>%
      add_legacy_name("L222.SubsectorLogit_en_CHINA") %>%
      add_precursors("L222.SubsectorLogit_en") ->
      L222.SubsectorLogit_en_CHINA

    L222.StubTech_en_CHINA %>%
      add_title("Stub technology map for CHINA energy provinces and sectors.") %>%
      add_units("NA") %>%
      add_comments("The stub technology table from L222.StubTech_en is filtered and repeated") %>%
      add_comments("for CHINA energy sectors in each state.") %>%
      add_legacy_name("L222.StubTech_en_CHINA") %>%
      add_precursors("L222.StubTech_en") ->
      L222.StubTech_en_CHINA

    L222.StubTechCoef_refining_CHINA %>%
      add_title("Refining stub tech coefficients for China energy provinces and sectors") %>%
      add_units("NA") %>%
      add_comments("Coefficients for refining stub technologies in L222.StubTechCoef_refining are filtered and repeated") %>%
      add_comments("for China energy sectors in each province.") %>%
      add_legacy_name("L222.StubTechCoef_refining_CHINA") %>%
      add_precursors("L222.StubTechCoef_refining") ->
      L222.StubTechCoef_refining_CHINA

    L222.GlobalTechSCurve_en_CHINA %>%
      add_title("Tech S curve parameters for China energy sectors.") %>%
      add_units("varies") %>%
      add_comments("S curve parameters from L222.GlobalTechScurve_en are filtered for China sectors.") %>%
      add_legacy_name("L222.GlobalTechSCurve_en_CHINA") %>%
      add_precursors("L222.GlobalTechSCurve_en") ->
      L222.GlobalTechSCurve_en_CHINA

    L222.GlobalTechCost_en_CHINA %>%
      add_title("Tech costs for China energy sectors.") %>%
      add_units("varies") %>%
      add_comments("Tech cost data from L222.GlobalTechCost_en are filtered for China sectors.") %>%
      add_legacy_name("L222.GlobalTechCost_en_CHINA") %>%
      add_precursors("L222.GlobalTechCost_en") ->
      L222.GlobalTechCost_en_CHINA

    L222.GlobalTechInterp_en_CHINA %>%
      add_title("Interpolation function key for China energy sectors.") %>%
      add_units("units") %>%
      add_comments("Interpolation function key from L222.GlobalTechInterp_en are filtered for China sectors.") %>%
      add_legacy_name("L222.GlobalTechInterp_en_CHINA") %>%
      add_precursors("L222.GlobalTechInterp_en") ->
      L222.GlobalTechInterp_en_CHINA

    L222.GlobalTechCoef_en_CHINA %>%
      add_title("Technology coefficients for China energy sectors.") %>%
      add_units("NA") %>%
      add_comments("Global technology coefficients from L222.GlobalTechCoef_en are filtered for China sectors.") %>%
      add_legacy_name("L222.GlobalTechCoef_en_CHINA") %>%
      add_precursors("L222.GlobalTechCoef_en") ->
      L222.GlobalTechCoef_en_CHINA

    L222.GlobalTechShrwt_en_CHINA %>%
      add_title("Technology shareweights for China energy sectors") %>%
      add_units("NA") %>%
      add_comments("Shareweights from L222.GlobalTechShrwt_en are filtered for China energy sectors.") %>%
      add_legacy_name("L222.GlobalTechShrwt_en_CHINA") %>%
      add_precursors("L222.GlobalTechShrwt_en") ->
      L222.GlobalTechShrwt_en_CHINA

    L222.GlobalTechCapture_en_CHINA %>%
      add_title("Carbon capture data for China energy sectors") %>%
      add_units("NA") %>%
      add_comments("Carbon capture data  from L222.GlobalTechCapture_en are filtered for China energy sectors.") %>%
      add_legacy_name("L222.GlobalTechCapture_en_CHINA") %>%
      add_precursors("L222.GlobalTechCapture_en") ->
      L222.GlobalTechCapture_en_CHINA

    L222.Supplysector_en_CHINA %>%
      add_title("Supplysector information, replace name of supplysector with the subsector names") %>%
      add_units("Varies") %>%
      add_comments("L222.Supplysector_en and L222.SubsectorLogit_en is repeated and filtered for use in China provinces.") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.Supplysector_en_CHINA") %>%
      add_precursors("L222.Supplysector_en",
                     "L222.SubsectorLogit_en") ->
      L222.Supplysector_en_CHINA

    L222.SubsectorShrwtFllt_en_CHINA %>%
      add_title("Subsector shareweights for energy in CHINA") %>%
      add_units("NA") %>%
      add_comments("CHINA energy subsector shareweights. There is no competition here, so shareweights are defaulted to 1.") %>%
      add_comments("Shareweights will be over-ridden by base year calibration.") %>%
      add_legacy_name("L222.SubsectorShrwtFllt_en_CHINA") %>%
      same_precursors_as(L222.SubsectorLogit_en_CHINA) ->
      L222.SubsectorShrwtFllt_en_CHINA

    L222.StubTechProd_refining_CHINA %>%
      add_title("China Calibrated fuel production by province.") %>%
      add_units("varies") %>%
      add_comments("Tech IDs where the calibration is identified as output in the calibrated_techs file are") %>%
      add_comments("are used to adjust data from L122.out_EJ_province_refining_F.") %>%
      add_legacy_name("L222.StubTechProd_refining_CHINA") %>%
      add_precursors("energy/calibrated_techs",
                     "L122.out_EJ_province_refining_F") ->
      L222.StubTechProd_refining_CHINA

    L222.StubTechMarket_en_CHINA %>%
      add_title("Market names of inputs to province refining sectors") %>%
      add_units("varies") %>%
      add_comments("Data from L222.GlobalTechCoef_en is adjusted for use in China provinces, depending") %>%
      add_comments("on whether regional markets are used.") %>%
      add_legacy_name("L222.StubTechMarket_en_CHINA") %>%
      add_precursors("L122.out_EJ_province_refining_F",
                     "L222.GlobalTechCoef_en") ->
      L222.StubTechMarket_en_CHINA

    L222.CarbonCoef_en_CHINA %>%
      add_title("Energy carbon coefficients in China") %>%
      add_units("varies") %>%
      add_comments("Carbon coefficients from L202.CarbonCoef are updated with China energy tech shareweights to") %>%
      add_comments("produce energy carbon coefficients in China.") %>%
      add_legacy_name("L222.CarbonCoef_en_China") %>%
      add_precursors("L222.SubsectorLogit_en",
                     "L202.CarbonCoef")  ->
      L222.CarbonCoef_en_CHINA

    return_data(L222.DeleteStubTech_CHINAen, L222.PassThroughSector_CHINAen, L222.Tech_CHINAen,
                L222.TechShrwt_CHINAen, L222.TechInterp_CHINAen, L222.TechCoef_CHINAen, L222.Production_CHINArefining,
                L222.Supplysector_en_CHINA, L222.SubsectorShrwtFllt_en_CHINA, L222.StubTechProd_refining_CHINA, L222.StubTechMarket_en_CHINA,
                L222.CarbonCoef_en_CHINA, L222.GlobalTechSCurve_en_CHINA,
                L222.GlobalTechCost_en_CHINA,
                L222.SubsectorLogit_en_CHINA,
                L222.StubTech_en_CHINA,
                L222.StubTechCoef_refining_CHINA,
                L222.GlobalTechInterp_en_CHINA,
                L222.GlobalTechCoef_en_CHINA,
                L222.GlobalTechShrwt_en_CHINA,
                L222.GlobalTechCapture_en_CHINA)

  } else {
    stop("Unknown command")
  }
}
