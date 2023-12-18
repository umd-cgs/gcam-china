# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L2323.detailed_industry_CHINA
#'
#' Make the logit and input tables for the detailed_industry sector in gcam-china
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2323.StubTechProd_detailed_industry},
#' \code{L2323.StubTechCoef_detailed_industry}, \code{L2323.StubTechCalInput_detailed_industry}, \code{L2323.StubTechMarket_detailed_industry}, \code{L2323.BaseService_detailed_industry},
#' \code{L2323.Supplysector_detailed_industry_CHINA}, \code{L2323.FinalEnergyKeyword_detailed_industry_CHINA}, \code{L2323.SubsectorLogit_detailed_industry_CHINA},
#' \code{L2323.SubsectorShrwtFllt_detailed_industry_CHINA}, \code{L2323.StubTech_detailed_industry_CHINA}, \code{L2323.PerCapitaBased_detailed_industry_CHINA},
#'  \code{L2323.PriceElasticity_detailed_industry_CHINA}, \code{L2323.IncomeElasticity_detailed_industry_gcam3_CHINA} , \code{L2323.GlobalTechCSeq_ind}.
#'  The corresponding file in the original data system was \code{L2323.detailed_industry_CHINA.R} (gcam-china level2).
#' @details Make the logit and input tables for the detailed_industry sector in gcam-china
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange distinct filter if_else group_by left_join mutate select
#' @importFrom tidyr gather spread
#' @author YangLiu  September 2020

module_gcamchina_L2323.detailed_industry_CHINA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "gcam-china/calibrated_techs",
             FILE = "gcam-china/A323.sector",
             FILE = "gcam-china/A323.subsector_interp",
             FILE = "gcam-china/A323.nonenergy_Cseq",
             FILE = "gcam-china/A323.subsector_logit",
             FILE = "gcam-china/A323.subsector_shrwt",
             FILE = "gcam-china/A323.globaltech_coef",
             FILE = "gcam-china/A323.globaltech_cost",
             FILE = "gcam-china/A323.globaltech_shrwt",
             FILE = "gcam-china/A323.globaltech_co2capture",
             FILE = "gcam-china/A323.demand",
             FILE = "gcam-china/A323.inc_elas_output",
             FILE = "gcam-china/A323.globaltech_retirement",
             FILE = "gcam-china/A323.sector_China",
             FILE = "gcam-china/A323.subsector_interp_China",
             FILE = "gcam-china/A323.subsector_logit_China",
             FILE = "gcam-china/A323.tech_coef_China",
             FILE = "gcam-china/A323.tech_shrwt_China",
             FILE = "gcam-china/A323.efficiency_improve",
             "L1323.out_Mt_province_detailed_industry_Yh",
             "L1323.IO_GJkg_province_detailed_industry_F_Yh",
             "L1323.in_EJ_province_detailed_industry_F_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2323.Supplysector_detailed_industry",
             "L2323.FinalEnergyKeyword_detailed_industry",
             "L2323.SubsectorLogit_detailed_industry",
             "L2323.SubsectorShrwtFllt_detailed_industry",
             "L2323.SubsectorInterp_detailed_industry",
             "L2323.StubTech_detailed_industry",
             "L2323.GlobalTechShrwt_detailed_industry",
             "L2323.GlobalTechCoef_detailed_industry",
             "L2323.GlobalTechCost_detailed_industry",
             "L2323.GlobalTechCapture_detailed_industry",
             "L2323.StubTechProd_detailed_industry",
             "L2323.StubTechMarket_detailed_industry",
             "L2323.StubTechCoef_detailed_industry",
             "L2323.PerCapitaBased_detailed_industry",
             "L2323.BaseService_detailed_industry",
             "L2323.PriceElasticity_detailed_industry",
             "L2323.IncomeElasticity_detailed_industry",
             "L2323.Supplysector_detailed_industry_China",
             "L2323.SubsectorLogit_detailed_industry_China",
             "L2323.SubsectorInterp_detailed_industry_China",
             "L2323.TechCoef_detailed_industry_China",
             "L2323.TechShrwt_detailed_industry_China",
             "L2323.Production_detailed_industry_China",
             "L2323.GlobalTechSCurve_detailed_industry",
             "L2323.GlobalTechProfitShutdown_detailed_industry",
             "L2323.GlobalTechCSeq_ind"))
  } else if(command == driver.MAKE) {


    all_data <- list(...)[[1]]
    # -----------------------------------------------------------------------------
    # 1. Read files
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = T)
    calibrated_techs <- get_data(all_data, "gcam-china/calibrated_techs", strip_attributes = T)

    A323.sector <- get_data(all_data, "gcam-china/A323.sector", strip_attributes = T)
    A323.subsector_interp <- get_data(all_data, "gcam-china/A323.subsector_interp", strip_attributes = T)
    #A323.subsector_interpTo <- get_data(all_data, "gcam-china/A323.subsector_interpTo", strip_attributes = T)
    A323.subsector_logit <- get_data(all_data, "gcam-china/A323.subsector_logit", strip_attributes = T)
    A323.subsector_shrwt <- get_data(all_data, "gcam-china/A323.subsector_shrwt", strip_attributes = T)
    A323.globaltech_coef <- get_data(all_data, "gcam-china/A323.globaltech_coef", strip_attributes = T)
    A323.globaltech_cost <- get_data(all_data, "gcam-china/A323.globaltech_cost", strip_attributes = T)
    A323.globaltech_shrwt <- get_data(all_data, "gcam-china/A323.globaltech_shrwt", strip_attributes = T)
    A323.globaltech_co2capture <- get_data(all_data, "gcam-china/A323.globaltech_co2capture", strip_attributes = T)
    A323.demand <- get_data(all_data, "gcam-china/A323.demand", strip_attributes = T)
    L1323.out_Mt_province_detailed_industry_Yh <- get_data(all_data, "L1323.out_Mt_province_detailed_industry_Yh", strip_attributes = T)
    L1323.IO_GJkg_province_detailed_industry_F_Yh <- get_data(all_data, "L1323.IO_GJkg_province_detailed_industry_F_Yh", strip_attributes = T)
    L1323.in_EJ_province_detailed_industry_F_Y <- get_data(all_data, "L1323.in_EJ_province_detailed_industry_F_Y", strip_attributes = T)
    A323.nonenergy_Cseq <- get_data(all_data, "gcam-china/A323.nonenergy_Cseq", strip_attributes = T)

    A323.inc_elas_output <- get_data(all_data, "gcam-china/A323.inc_elas_output", strip_attributes = T)

    A323.efficiency_improve <- get_data(all_data, "gcam-china/A323.efficiency_improve", strip_attributes = T)

    A323.globaltech_retirement <- get_data(all_data, "gcam-china/A323.globaltech_retirement", strip_attributes = T)

    # China region tables
    A323.sector_China <- get_data(all_data, "gcam-china/A323.sector_China", strip_attributes = T)
    A323.subsector_interp_China <- get_data(all_data, "gcam-china/A323.subsector_interp_China", strip_attributes = T)
    A323.subsector_logit_China <- get_data(all_data, "gcam-china/A323.subsector_logit_China", strip_attributes = T)
    A323.tech_coef_China <- get_data(all_data, "gcam-china/A323.tech_coef_China", strip_attributes = T)
    A323.tech_shrwt_China <- get_data(all_data, "gcam-china/A323.tech_shrwt_China", strip_attributes = T)


   # detailed_industry_CCS_cost_2000USDtCO2 <- 80
    #??CCS?龰
    detailed_industry_CCS_cost_2000USDtCO2 <- 40
    CO2_storage_cost_1990USDtC <- 42
    CO2_coef_detailed_industry <- 0.33
    # -----------------------------------------------------------------------------
    # 2. Perform computations
     "NOTE: detailed_industry sectors are only created in provinces where the Census data indicate production"
    detailed_industry_provinces <- unique( L1323.out_Mt_province_detailed_industry_Yh$province )
    # ===================================================
    # 0. Give binding for variable names used in pipeline
    year <- value <- GCAM_region_ID <- sector <- fuel <- NULL

    # ===================================================
    # 1. Perform computations
    # Create tables to delete technologies and subsectors in regions where heat is not modeled as a fuel
    # 1a. Supplysector information
    # L2323.Supplysector_detailed_industry: Supply sector information for detailed_industry sector
    A323.sector %>%
      write_to_all_provinces(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME),province = province_names_mappings$province)  ->
      L2323.Supplysector_detailed_industry


    # L2323.FinalEnergyKeyword_detailed_industry: Supply sector keywords for detailed_industry sector
    A323.sector %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]],province = province_names_mappings$province) %>%
      na.omit ->
      L2323.FinalEnergyKeyword_detailed_industry

    # 1b. Subsector information
    # L2323.SubsectorLogit_detailed_industry: Subsector logit exponents of detailed_industry sector
    #
    A323.subsector_logit %>%
      write_to_all_provinces(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME),province = province_names_mappings$province)%>%
      select(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)) ->
      L2323.SubsectorLogit_detailed_industry

    # and L2323.SubsectorShrwtFllt_detailed_industry: Subsector shareweights of detailed_industry sector
    A323.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]],province = province_names_mappings$province) ->
      L2323.SubsectorShrwtFllt_detailed_industry

    # L2323.SubsectorInterp_detailed_industry: Subsector shareweight interpolation of detailed_industry sector
    A323.subsector_interp %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["SubsectorInterp"]],province = province_names_mappings$province) ->
      L2323.SubsectorInterp_detailed_industry

    #L2323.SubsectorInterpTo_detailed_industry <- set_years(A323.subsector_interpTo[rep(1:nrow(A323.subsector_logit),
    #                                                                                          times = length(detailed_industry_provinces)),] %>%
    #                                                      write_to_all_provinces(names_SubsectorInterpTo) %>%
    #                                                            select_(.dots = names_SubsectorInterpTo))


    # 1c. Technology information
    # L2323.StubTech_detailed_industry: Identification of stub technologies of detailed_industry
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A323.globaltech_shrwt %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["Tech"]],province = province_names_mappings$province) %>%
      rename(stub.technology = technology) ->
      L2323.StubTech_detailed_industry

    # L2323.GlobalTechShrwt_detailed_industry: Shareweights of global detailed_industry technologies
    A323.globaltech_shrwt %>%
      gather(year, value, -supplysector, -subsector, -technology) %>%
      mutate(year = as.integer(sub("X", "", year))) %>%
      complete(nesting(supplysector, subsector, technology), year = MODEL_YEARS) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      ungroup %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight" )) ->
      L2323.GlobalTechShrwt_detailed_industry

    # L2323.GlobalTechCoef_detailed_industry: Energy inputs and coefficients of detailed_industry technologies
    A323.globaltech_coef %>%
      gather(year, value, -supplysector, -subsector, -technology, -minicam.energy.input) %>%
      mutate(year = as.integer(sub("X", "", year))) %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = MODEL_YEARS) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, value, rule = 1),
             coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      ungroup %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L2323.GlobalTechCoef_detailed_industry

    # Carbon capture rates from technologies with CCS
    # L2323.GlobalTechCapture_detailed_industry: CO2 capture fractions from global detailed_industry production technologies with CCS
    # No need to consider historical periods or intermittent technologies here
    A323.globaltech_co2capture %>%
      gather(year, value, -supplysector, -subsector, -technology) %>%
      mutate(year = as.integer(sub("X", "", year))) %>%
      complete(nesting(supplysector, subsector, technology), year = MODEL_YEARS) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(year, value, rule = 1),
             remove.fraction = round(remove.fraction, energy.DIGITS_REMOVE.FRACTION)) %>%
      ungroup %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(c(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "remove.fraction" )) %>%
      mutate(storage.market = energy.CO2.STORAGE.MARKET) ->
      L2323.GlobalTechCapture_detailed_industry

    # L2323.GlobalTechCost_detailed_industry: Non-energy costs of global detailed_industry manufacturing technologies
    A323.globaltech_cost %>%
      gather(year, value, -supplysector, -subsector, -technology, -minicam.non.energy.input) %>%
      mutate(year = as.integer(sub("X", "", year))) %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input), year = MODEL_YEARS) %>%
      arrange(supplysector, subsector, technology, minicam.non.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, value, rule = 1),
             input.cost = round(input.cost, energy.DIGITS_COST)) %>%
      ungroup %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]]) ->
      L2323.GlobalTechCost_detailed_industry # intermediate tibble


    A323.nonenergy_Cseq %>%
      repeat_add_columns(tibble(year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS))) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCSeq"]]) ->
      L2323.GlobalTechCSeq_ind

    #Retirement information
    L2323.globaltech_retirement <- set_years ( A323.globaltech_retirement )
    L2323.globaltech_retirement[ c( "sector.name", "subsector.name" ) ] <- L2323.globaltech_retirement[ c( "supplysector", "subsector" ) ]

    #Copy the data in the first future period through to the end year
    L2323.globaltech_retirement <- rbind(
      subset( L2323.globaltech_retirement, year == MODEL_FINAL_BASE_YEAR ),
      repeat_add_columns( L2323.globaltech_retirement %>% filter(year == min(MODEL_FUTURE_YEARS)) %>% mutate(year = NULL ), tibble::tibble(year = MODEL_FUTURE_YEARS ) ))

    #Retirement may consist of any of three types of retirement function (phased, s-curve, or none)
    # All of these options have different headers, and all are allowed
    if( any( !is.na( L2323.globaltech_retirement$shutdown.rate ) ) ){
      # "L2323.GlobalTechShutdown_detailed_industry: Global tech lifetime and shutdown rate"
      L2323.GlobalTechShutdown_detailed_industry <- L2323.globaltech_retirement[
        !is.na( L2323.globaltech_retirement$shutdown.rate ),
        c( LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "shutdown.rate" ) ]
    }

      #"L2323.GlobalTechSCurve_detailed_industry: Global tech lifetime and s-curve retirement function"
      L2323.GlobalTechSCurve_detailed_industry <- L2323.globaltech_retirement[
        !is.na( L2323.globaltech_retirement$half.life ),
        c( LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime", "steepness", "half.life" ) ]

    if( any( is.na( L2323.globaltech_retirement$shutdown.rate ) & is.na( L2323.globaltech_retirement$half.life ) ) ){
      # "L2323.GlobalTechLifetime_detailed_industry: Global tech lifetime"
      L2323.GlobalTechLifetime_detailed_industry <- L2323.globaltech_retirement[
        is.na( L2323.globaltech_retirement$shutdown.rate ) & is.na( L2323.globaltech_retirement$half.life ),
        c( LEVEL2_DATA_NAMES[["GlobalTechYr"]], "lifetime" ) ]
    }

      # "L2323.GlobalTechProfitShutdown_detailed_industry: Global tech profit shutdown decider"
      L2323.GlobalTechProfitShutdown_detailed_industry <- L2323.globaltech_retirement[
        !is.na( L2323.globaltech_retirement$median.shutdown.point ),
        c( LEVEL2_DATA_NAMES[["GlobalTechYr"]], "median.shutdown.point", "profit.shutdown.steepness") ]


    # We don't calculate detailed_industry CCS cost at now
    if(1==0){
    # Note: adjusting non-energy costs of technologies with CCS to include CO2 capture costs
    #       The additional CCS-related non-energy costs are not included in the global technology assessment.
    detailed_industry_CCS_cost_total_1975USDtC <- detailed_industry_CCS_cost_2000USDtCO2 * conv_2000_1975_USD * conv_C_CO2
    CO2_storage_cost_1975USDtC <- CO2_storage_cost_1990USDtC * conv_1990_1975_USD
    detailed_industry_CCS_cost_1975USDtC <- detailed_industry_CCS_cost_total_1975USDtC - CO2_storage_cost_1975USDtC

    L2323.GlobalTechCapture_detailed_industry %>%
      pull(remove.fraction) %>%
      mean -> detailed_industry_CO2_capture_frac


    CO2stored_IO_kgCkgdetailed_industry <- CO2_coef_detailed_industry * detailed_industry_CO2_capture_frac
    detailed_industry_CCS_cost_75USD_detailed_industry <- detailed_industry_CCS_cost_1975USDtC * CO2stored_IO_kgCkgdetailed_industry / conv_t_kg

    # Adjust the non-energy costs in the table for model input
    L2323.GlobalTechCost_detailed_industry %>%
      filter(technology %in% L2323.GlobalTechCapture_detailed_industry[["technology"]]) %>%
      mutate(input.cost = input.cost + detailed_industry_CCS_cost_75USD_detailed_industry) %>%
      bind_rows(filter(L2323.GlobalTechCost_detailed_industry, !(technology %in% L2323.GlobalTechCapture_detailed_industry[["technology"]]))) %>%
      mutate(input.cost = round(input.cost, 7)) ->
      L2323.GlobalTechCost_detailed_industry
    }

    # Calibration and region-specific data
    # L2323.StubTechProd_detailed_industry: calibrated detailed_industry production
    calibrated_techs %>%
      filter(calibration == "output") %>% # Only take the tech IDs where the calibration is identified as output
      select(sector, supplysector, subsector, technology) %>%
      distinct ->
      calibrated_techs_export # temporary tibble

    L1323.out_Mt_province_detailed_industry_Yh %>%
      complete(nesting(supplysector, subsector, technology), province = detailed_industry_provinces,
               year = MODEL_BASE_YEARS) %>%
      mutate(value = replace_na(value,0)) %>%
      mutate(calOutputValue = round(value, energy.DIGITS_CALOUTPUT),value = NULL,region = province) %>%
      set_subsector_shrwt %>%
      mutate(stub.technology = technology,
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L2323.StubTechProd_detailed_industry



    # L2323.StubTechCoef_detailed_industry: region-specific coefficients of detailed_industry production technologies
    # Take this as a given in all years for which data is available
    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct ->
      calibrated_techs_export # temporary tibble

    L1323.IO_GJkg_province_detailed_industry_F_Yh %>%
      complete(nesting(supplysector, subsector, technology,minicam.energy.input),
               province = province_names_mappings$province,
               year = MODEL_YEARS) %>%
      group_by(supplysector, subsector, technology,minicam.energy.input,province) %>%
      mutate(coefficient = if_else(year == 1975,0,coefficient)) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% c( MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      mutate(coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      mutate(region = province,
             stub.technology = technology,
             market.name = region) %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamchina.REGIONAL_FUEL_MARKETS,
                                   region, market.name)) %>%
      #NOTE: electricity is consumed from province markets
      mutate(market.name = if_else(minicam.energy.input %in% gcamchina.PROVINCE_FUEL_MARKETS,
                                   region, market.name)) %>%
      mutate(market.name = if_else(minicam.energy.input %in% c("delivered biomass","coke"),
                                   "China", market.name)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L2323.StubTechCoef_detailed_industry


    #Include the efficiency improvement

    L2323.StubTechCoef_detailed_industry %>%
      left_join(A323.efficiency_improve, by =c( "supplysector","subsector","stub.technology","minicam.energy.input","year")) %>%
      mutate(coefficient = coefficient * improvement ) %>%
      group_by(region,supplysector, subsector, stub.technology,minicam.energy.input,market.name) %>%
      mutate(coefficient = approx_fun(year, coefficient, rule = 2)) %>%
      ungroup() %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L2323.StubTechCoef_detailed_industry


    # L2323.StubTechCalInput_detailed_industry: calibrated detailed_industry production
    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct ->
      calibrated_techs_export # temporary tibble

    L1323.in_EJ_province_detailed_industry_F_Y %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      mutate(stub.technology = technology,
             share.weight.year = year,
             subs.share.weight = if_else(calibrated.value > 0, 1, 0),
             tech.share.weight = subs.share.weight,
             region = province) %>%
      select(region, supplysector, subsector, stub.technology, year, minicam.energy.input,
             calibrated.value, share.weight.year, subs.share.weight, tech.share.weight) ->
      L2323.StubTechCalInput_detailed_industry

    # L2323.StubTechMarket_detailed_industry: Name the markets for the fuels consumed by the province detailed_industry sectors" )
    L2323.StubTech_detailed_industry %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      left_join(calibrated_techs %>%select(c("supplysector","subsector","technology","minicam.energy.input")), by =c( "supplysector","subsector", stub.technology = "technology")) %>%
      filter(!is.na(minicam.energy.input)) %>%
      filter(!(minicam.energy.input %in% c("coke"))) %>%
      mutate(market.name = region) %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamchina.REGIONAL_FUEL_MARKETS,
                                   region, market.name)) %>%
      #NOTE: electricity is consumed from province markets
      mutate(market.name = if_else(minicam.energy.input %in% gcamchina.PROVINCE_FUEL_MARKETS,
                                   region, market.name)) %>%
      mutate(market.name = if_else(minicam.energy.input %in% c("delivered biomass","coke"),
                                   "China", market.name)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechMarket"]]) ->
      L2323.StubTechMarket_detailed_industry

    # L2323.PerCapitaBased_detailed_industry: per-capita based flag for detailed_industry exports final demand
    A323.demand %>%
      mutate(region = "China") %>%
      select(LEVEL2_DATA_NAMES[["PerCapitaBased"]]) ->
      L2323.PerCapitaBased_detailed_industry

    # L2323.BaseService_detailed_industry: base-year service output of detailed_industry
    sector_list = c("IRONSTL","construction","mining energy use",
                    "agriculture energy use","chemicals")

    L2323.StubTechProd_detailed_industry %>%
      filter(supplysector %in% sector_list) %>%
      group_by(year, supplysector) %>%
      summarise(base.service = sum(calOutputValue)) %>%
      ungroup() %>%
      mutate(region = "China") %>%
      rename(energy.final.demand = supplysector) %>%
      select(LEVEL2_DATA_NAMES[["BaseService"]]) ->
      L2323.BaseService_detailed_industry

    # L2323.PriceElasticity_detailed_industry: price elasticity
    A323.demand %>%
      repeat_add_columns(tibble::tibble(year = MODEL_FUTURE_YEARS)) %>%
      mutate(region = "China") %>%
      select(LEVEL2_DATA_NAMES[["PriceElasticity"]]) ->
      L2323.PriceElasticity_detailed_industry

    A323.inc_elas_output %>%
      gather(year, value, -supplysector) %>%
      mutate(year = as.integer(sub("X", "", year))) %>%
      rename(income.elasticity = value, energy.final.demand = supplysector) %>%
      complete(nesting(energy.final.demand), year = MODEL_FUTURE_YEARS) %>%
      #mutate(income.elasticity = approx_fun(year, income.elasticity, rule = 2)) %>%
      mutate(region = "China") %>%
      select(LEVEL2_DATA_NAMES[["IncomeElasticity"]]) ->
      L2323.IncomeElasticity_detailed_industry

    # Pass-through sector, subsector, and technology information for the China region, consuming the output of the provinces
    A323.sector_China$region <- "China"
    A323.sector_China$logit.year.fillout <- min(MODEL_BASE_YEARS)

    province_no_HK_MC <- province_names_mappings %>% filter(!(province %in% c("HK","MC")))

    A323.sector_China %>% select(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME)) -> L2323.Supplysector_detailed_industry_China

    L2323.SubsectorLogit_detailed_industry_China <- set_years(A323.subsector_logit_China[rep(1:nrow(A323.subsector_logit_China),
                                              times = length(province_no_HK_MC$province)),]%>%

                mutate(subsector = paste(rep(province_no_HK_MC$province,
                                             each = nrow(A323.subsector_logit_China)), subsector, sep = " "),
                       region = "China",logit.year.fillout = 1975) %>%
    select(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME)))


    # Subsector logit tables
    L2323.SubsectorInterp_detailed_industry_China <- set_years(A323.subsector_interp_China[rep(1:nrow(A323.subsector_logit_China),
                                                                                               each = length(province_no_HK_MC$province)),] %>%
      mutate(subsector = paste(province_no_HK_MC$province, subsector, sep = " "),
             region = "China") %>%
      select(LEVEL2_DATA_NAMES[["SubsectorInterp"]]))

    # Technology info for the China pass-through detailed_industry sectors
    L2323.TechCoef_detailed_industry_China <-
      repeat_add_columns(A323.tech_coef_China, tibble::tibble(market.name = province_names_mappings$province)) %>%
      filter(!(market.name %in% c("HK","MC"))) %>%
      gather(year, value, -supplysector,-subsector,-technology,-minicam.energy.input,-market.name) %>%
      mutate(year = as.integer(sub("X", "", year))) %>%
      mutate(subsector = paste(market.name, subsector, sep = " "),
             technology = subsector,
             region = "China") %>%
      complete(nesting(region,supplysector,subsector,technology,minicam.energy.input,market.name), year = MODEL_YEARS) %>%
      mutate(coefficient = approx_fun(year, value, rule = 1)) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]])

    L2323.TechShrwt_detailed_industry_China <- repeat_add_columns(A323.tech_shrwt_China, tibble::tibble(province = province_names_mappings$province)) %>%
      gather(year, value, -supplysector,-subsector,-technology,-province) %>%
      mutate(subsector = paste(province, subsector, sep = " "),
             technology = subsector,
             region = "China") %>%
      mutate(year = as.integer(sub("X", "", year))) %>%
      complete(nesting(region,supplysector,subsector,technology,province), year = MODEL_YEARS) %>%
      filter(!(province %in% c("HK","MC"))) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]])

    #Technology calibration - use the technology-level table from the provinces, aggregated by sector and province
    L2323.Output_detailed_industry_by_province <- group_by(L2323.StubTechProd_detailed_industry, region, supplysector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup() %>%
      mutate(subsector = paste(region, supplysector, sep = " ")) %>%
      select(-region)

    L2323.Production_detailed_industry_China <- subset(L2323.TechShrwt_detailed_industry_China, year %in% MODEL_BASE_YEARS) %>%
      select(-share.weight) %>%
      left_join(L2323.Output_detailed_industry_by_province, by = c( "supplysector", "subsector", "year")) %>%

      mutate(calOutputValue= replace_na(calOutputValue,0)) %>%
      set_subsector_shrwt() %>%
      mutate(share.weight.year = year,
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # ===================================================
    # Produce outputs
    L2323.Supplysector_detailed_industry %>%
      add_title("detailed_industry supplysector") %>%
      add_units("NA") %>%
      add_comments("Added detailed_industry supplysector") %>%
      add_legacy_name("L2323.Supplysector_detailed_industry") %>%
      add_precursors("gcam-china/calibrated_techs", "gcam-china/A323.sector") ->
      L2323.Supplysector_detailed_industry

    L2323.FinalEnergyKeyword_detailed_industry %>%
      add_title("detailed_industry FinalEnergyKeyword") %>%
      add_units("NA") %>%
      add_comments("detailed_industry FinalEnergyKeyword") %>%
      add_legacy_name("L2323.FinalEnergyKeyword_detailed_industry") %>%
      add_precursors("gcam-china/A323.sector") ->
      L2323.FinalEnergyKeyword_detailed_industry

    L2323.SubsectorLogit_detailed_industry %>%
      add_title("Subsector logit exponents for detailed_industry sector") %>%
      add_units("NA") %>%
      add_comments("Subsector logit exponents") %>%
      add_legacy_name("L2323.SubsectorLogit_detailed_industry") %>%
      add_precursors("gcam-china/A323.subsector_logit", "gcam-china/province_names_mappings") ->
      L2323.SubsectorLogit_detailed_industry

if( exists( "L2323.SubsectorShrwt_detailed_industry" ) ){
    L2323.SubsectorShrwt_detailed_industry %>%
      add_title("Subsector shareweights of detailed_industry sector in steel producing provinces") %>%
      add_units("NA") %>%
      add_comments("Expanded subsector shareweights") %>%
      add_legacy_name("L2323.SubsectorShrwt_detailed_industry") %>%
      add_precursors("gcam-china/A323.SubsectorShrwt") ->
      L2323.SubsectorShrwt_detailed_industry
	}

    L2323.SubsectorShrwtFllt_detailed_industry %>%
      add_title("Subsector shareweights of detailed_industry sector") %>%
      add_units("NA") %>%
      add_comments("Subsector shareweights") %>%
      add_legacy_name("L2323.SubsectorShrwtFllt_detailed_industry") %>%
      add_precursors("gcam-china/A323.subsector_shrwt") ->
      L2323.SubsectorShrwtFllt_detailed_industry

if( exists( "L2323.SubsectorInterp_detailed_industry" ) ) {
    L2323.SubsectorInterp_detailed_industry %>%
      add_title("Subsector shareweight interpolation of detailed_industry sector") %>%
      add_units("NA") %>%
      add_comments("Expanded subsector shareweight interpolation to detailed_industry producing provinces in region CHINA") %>%
      add_legacy_name("L2323.SubsectorInterp_detailed_industry") %>%
      add_precursors("gcam-china/A323.subsector_interp") ->
      L2323.SubsectorInterp_detailed_industry
	}

if( exists( "L2323.SubsectorInterpTo_detailed_industry" ) ) {
    L2323.SubsectorInterpTo_detailed_industry %>%
      add_title("Subsector shareweight interpolation of detailed_industry sector") %>%
      add_units("NA") %>%
      add_comments("Expanded subsector shareweight interpolation to detailed_industry producing provinces in region CHINA") %>%
      add_legacy_name("L2323.SubsectorInterpTo_detailed_industry") %>%
      add_precursors("gcam-china/A323.subsector_interpTo") ->
      L2323.SubsectorInterpTo_detailed_industry
	}

    L2323.StubTech_detailed_industry %>%
      add_title("Identification of stub technologies of detailed_industry sector") %>%
      add_units("Unitless") %>%
      add_comments("Expanded identification of stub technologies of detailed_industry") %>%
      add_legacy_name("L2323.StubTech_detailed_industry") %>%
      add_precursors("gcam-china/A323.globaltech_shrwt") ->
      L2323.StubTech_detailed_industry

    L2323.GlobalTechShrwt_detailed_industry %>%
      add_title("Supply sector keywords for detailed_industry sector in detailed_industry producing provinces") %>%
      add_units("Unitless") %>%
      add_comments("GlobalTech shareweight detailed_industry producing provinces in region CHINA") %>%
      add_legacy_name("L2323.GlobalTechShrwt_detailed_industry") %>%
      add_precursors("gcam-china/A323.globaltech_shrwt") ->
      L2323.GlobalTechShrwt_detailed_industry

    L2323.GlobalTechCoef_detailed_industry %>%
      add_title("Energy inputs and coefficients of detailed_industry technologies") %>%
      add_units("Gj/kg") %>%
      add_comments("Energy inputs and coefficients of detailed_industry technologies") %>%
      add_legacy_name("L2323.GlobalTechCoef_detailed_industry") %>%
      add_precursors("gcam-china/A323.globaltech_coef") ->
      L2323.GlobalTechCoef_detailed_industry

    L2323.GlobalTechCost_detailed_industry %>%
      add_title("Non-energy costs of global detailed_industry manufacturing technologies") %>%
      add_units("1990$") %>%
      add_comments("Non-energy costs of global detailed_industry manufacturing technologies") %>%
      add_legacy_name("L2323.GlobalTechCost_detailed_industry") %>%
      add_precursors("gcam-china/A323.globaltech_cost") ->
      L2323.GlobalTechCost_detailed_industry

    L2323.GlobalTechCSeq_ind %>%
      add_title("CO2 capture fractions from global electricity generation technologies") %>%
      add_units("Unitless") %>%
      add_comments("Remove fractions from A323.nonenergy_Cseq are expanded into all model years") %>%
      add_legacy_name("L2323.GlobalTechCSeq_ind") %>%
      add_precursors("gcam-china/A323.nonenergy_Cseq") ->
      L2323.GlobalTechCSeq_ind

    L2323.GlobalTechCapture_detailed_industry %>%
      add_title(" CO2 capture fractions from global detailed_industry production technologies with CCS") %>%
      add_units("%") %>%
      add_comments(" CO2 capture fractions from global detailed_industry production technologies with CCS") %>%
      add_legacy_name("L2323.GlobalTechCapture_detailed_industry") %>%
      add_precursors("gcam-china/A323.globaltech_co2capture") ->
      L2323.GlobalTechCapture_detailed_industry

    L2323.StubTechProd_detailed_industry %>%
      add_title("Calibrate detailed_industry production in region CHINA") %>%
      add_units("Mt") %>%
      add_comments("Calibrate detailed_industry production in region CHINA") %>%
      add_legacy_name("L2323.StubTechProd_detailed_industry") %>%
      add_precursors("L1323.out_Mt_province_detailed_industry_Yh", "gcam-china/calibrated_techs") ->
      L2323.StubTechProd_detailed_industry

    L2323.StubTechMarket_detailed_industry %>%
      add_title("Name the markets for the fuels consumed by the province detailed_industry sectors") %>%
      add_units("Unitless") %>%
      add_comments("Name the markets for the fuels consumed by the province detailed_industry sectors") %>%
      add_legacy_name("L2323.StubTechMarket_detailed_industry") %>%
      add_precursors("gcam-china/A323.globaltech_shrwt", "gcam-china/calibrated_techs") ->
      L2323.StubTechMarket_detailed_industry

    L2323.StubTechCoef_detailed_industry %>%
      add_title("region-specific coefficients of detailed_industry production technologies") %>%
      add_units("Gj/kg") %>%
      add_comments("region-specific coefficients of detailed_industry production technologies") %>%
      add_legacy_name("L2323.StubTechCoef_detailed_industry") %>%
      add_precursors("L1323.IO_GJkg_province_detailed_industry_F_Yh", "gcam-china/calibrated_techs","gcam-china/A323.efficiency_improve","L1323.in_EJ_province_detailed_industry_F_Y") ->
      L2323.StubTechCoef_detailed_industry

    L2323.PerCapitaBased_detailed_industry %>%
      add_title("Per-capita based flag for detailed_industry exports final demand") %>%
      add_units("Unitless") %>%
      add_comments("Per-capita based flag for detailed_industry exports final demand") %>%
      add_legacy_name("L2323.PerCapitaBased_detailed_industry") %>%
      add_precursors("gcam-china/A323.demand", "gcam-china/calibrated_techs") ->
      L2323.PerCapitaBased_detailed_industry

	L2323.BaseService_detailed_industry %>%
      add_title("Base-year service output of detailed_industry") %>%
      add_units("Mt") %>%
      add_comments("Base-year service output of detailed_industry") %>%
      add_legacy_name("L2323.BaseService_detailed_industry") %>%
      add_precursors("L1323.out_Mt_province_detailed_industry_Yh") ->
      L2323.BaseService_detailed_industry

    L2323.PriceElasticity_detailed_industry %>%
      add_title("Price elasticity for detailed_industry in detailed_industry producing provinces") %>%
      add_units("Unitless") %>%
      add_comments("Expanded price elasticity for detailed_industry to detailed_industry producing provinces in region CHINA") %>%
      add_legacy_name("L2323.PriceElasticity_detailed_industry") %>%
      add_precursors("gcam-china/A323.demand") ->
      L2323.PriceElasticity_detailed_industry

    L2323.IncomeElasticity_detailed_industry %>%
      add_title("Expanded Income elasticity for detailed_industry to detailed_industry producing provinces in region CHINA") %>%
      add_units("Unitless") %>%
      add_comments("Expanded Income elasticity for detailed_industry to detailed_industry producing provinces in region CHINA") %>%
      add_legacy_name("L2323.IncomeElasticity_detailed_industry") %>%
      add_precursors("gcam-china/A323.inc_elas_output") ->
      L2323.IncomeElasticity_detailed_industry

    L2323.Supplysector_detailed_industry_China %>%
      add_title("Price elasticity for detailed_industry in detailed_industry producing provinces",overwrite = TRUE) %>%
      add_units("Unitless") %>%
      add_comments("Price elasticity for detailed_industry to detailed_industry producing provinces in region CHINA") %>%
      add_legacy_name("L2323.Supplysector_detailed_industry_China") %>%
      add_precursors("gcam-china/calibrated_techs") ->
      L2323.Supplysector_detailed_industry_China

    L2323.SubsectorLogit_detailed_industry_China %>%
      add_title("Subsector logit exponents for detailed_industry sector in China") %>%
      add_units("Unitless") %>%
      add_comments("Subsector logit exponents for detailed_industry sector in China") %>%
      add_legacy_name("L2323.SubsectorLogit_detailed_industry_China") %>%
      add_precursors("gcam-china/A323.subsector_logit_China") ->
      L2323.SubsectorLogit_detailed_industry_China

    L2323.SubsectorInterp_detailed_industry_China %>%
      add_title("Subsector shareweight interpolation of detailed_industry sector in China") %>%
      add_units("Unitless") %>%
      add_comments("Subsector shareweight interpolation of detailed_industry sector in China") %>%
      add_legacy_name("L2323.SubsectorInterp_detailed_industry_China") %>%
      add_precursors("gcam-china/A323.subsector_interp_China") ->
      L2323.SubsectorInterp_detailed_industry_China

    L2323.TechCoef_detailed_industry_China %>%
      add_title("Technology info for the China pass-through detailed_industry sectors") %>%
      add_units("Unitless") %>%
      add_comments("Technology info for the China pass-through detailed_industry sectors") %>%
      add_legacy_name("L2323.TechCoef_detailed_industry_China") %>%
      add_precursors("gcam-china/A323.tech_coef_China") ->
      L2323.TechCoef_detailed_industry_China

    L2323.TechShrwt_detailed_industry_China %>%
      add_title("Technology shareweight for the China pass-through detailed_industry sectors") %>%
      add_units("Unitless") %>%
      add_comments("Technology shareweight for the China pass-through detailed_industry sectors") %>%
      add_legacy_name("L2323.TechShrwt_detailed_industry_China") %>%
      add_precursors("gcam-china/A323.tech_shrwt_China") ->
      L2323.TechShrwt_detailed_industry_China

    L2323.Production_detailed_industry_China %>%
      add_title("Steel Production in region CHINA") %>%
      add_units("Unitless") %>%
      add_comments("Steel Production in region CHINA") %>%
      add_legacy_name("L2323.Production_detailed_industry_China") %>%
      add_precursors("L1323.out_Mt_province_detailed_industry_Yh", "gcam-china/calibrated_techs") ->
      L2323.Production_detailed_industry_China


if( exists( "L2323.GlobalTechShutdown_detailed_industry" ) ) {
    L2323.GlobalTechShutdown_detailed_industry %>%
      add_title("GlobalTechShutdown") %>%
      add_units("Unitless") %>%
      add_comments("GlobalTechShutdown") %>%
      add_legacy_name("L2323.GlobalTechShutdown_detailed_industry") %>%
      add_precursors("gcam-china/A323.globaltech_retirement") ->
      L2323.GlobalTechShutdown_detailed_industry
}
if( exists( "L2323.GlobalTechSCurve_detailed_industry" ) ) {
    L2323.GlobalTechSCurve_detailed_industry %>%
      add_title("Global tech lifetime and s-curve retirement function") %>%
      add_units("Unitless") %>%
      add_comments("Global tech lifetime and s-curve retirement function") %>%
      add_legacy_name("L2323.GlobalTechSCurve_detailed_industry") %>%
      add_precursors("gcam-china/A323.globaltech_retirement") ->
      L2323.GlobalTechSCurve_detailed_industry
}
if( exists( "L2323.GlobalTechLifetime_detailed_industry" ) ) {
    L2323.GlobalTechLifetime_detailed_industry %>%
      add_title("Global tech lifetime") %>%
      add_units("Unitless") %>%
      add_comments("Global tech lifetime") %>%
      add_legacy_name("L2323.GlobalTechLifetime_detailed_industry") %>%
      add_precursors("gcam-china/A323.globaltech_retirement") ->
      L2323.GlobalTechLifetime_detailed_industry
}
if( exists( "L2323.GlobalTechProfitShutdown_detailed_industry" ) ) {
    L2323.GlobalTechProfitShutdown_detailed_industry %>%
      add_title("Global tech profit shutdown decider") %>%
      add_units("Unitless") %>%
      add_comments("Global tech profit shutdown decider") %>%
      add_legacy_name("L2323.GlobalTechProfitShutdown_detailed_industry") %>%
      add_precursors("gcam-china/A323.globaltech_retirement") ->
      L2323.GlobalTechProfitShutdown_detailed_industry
}

    return_data(L2323.Supplysector_detailed_industry, L2323.FinalEnergyKeyword_detailed_industry,
                L2323.SubsectorLogit_detailed_industry, L2323.SubsectorShrwtFllt_detailed_industry,
                L2323.SubsectorInterp_detailed_industry, L2323.StubTech_detailed_industry,
                L2323.GlobalTechShrwt_detailed_industry, L2323.GlobalTechCoef_detailed_industry,
                L2323.GlobalTechCost_detailed_industry, L2323.GlobalTechCapture_detailed_industry,
                L2323.StubTechProd_detailed_industry, L2323.StubTechMarket_detailed_industry,
                L2323.StubTechCoef_detailed_industry, L2323.PerCapitaBased_detailed_industry,
                L2323.BaseService_detailed_industry, L2323.PriceElasticity_detailed_industry,
                L2323.IncomeElasticity_detailed_industry, L2323.Supplysector_detailed_industry_China,
                L2323.SubsectorLogit_detailed_industry_China, L2323.SubsectorInterp_detailed_industry_China,
                L2323.TechCoef_detailed_industry_China, L2323.TechShrwt_detailed_industry_China,L2323.Production_detailed_industry_China,
			        	L2323.GlobalTechSCurve_detailed_industry,L2323.GlobalTechProfitShutdown_detailed_industry,L2323.GlobalTechCSeq_ind )

  } else {
    stop("Unknown command")
  }
}

