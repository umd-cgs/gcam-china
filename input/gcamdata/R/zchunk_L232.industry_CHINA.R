# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L232.industry_CHINA
#'
#' Prepare level 2 industry sector files for CHINA.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L232.DeleteSupplysector_CHINAind}, \code{L232.DeleteFinalDemand_CHINAind}, \code{L232.DeleteDomSubsector_CHINAind},
#' \code{L232.DeleteTraSubsector_CHINAind}, \code{L232.Production_reg_imp_CHINA}, \code{L232.BaseService_iron_steel_CHINA},
#' \code{L232.StubTechCalInput_indenergy_CHINA}, \code{L232.StubTechCalInput_indfeed_CHINA}, \code{L232.StubTechProd_industry_CHINA},
#' \code{L232.StubTechCoef_industry_CHINA}, \code{L232.StubTechMarket_ind_CHINA}, \code{L232.StubTechSecMarket_ind_CHINA},
#' \code{L232.BaseService_ind_CHINA}, \code{L232.DeleteSubsector_ind_CHINA}, \code{L232.Supplysector_ind_CHINA},
#' \code{L232.FinalEnergyKeyword_ind_CHINA}, \code{L232.SubsectorLogit_ind_CHINA}, \code{L232.SubsectorShrwtFllt_ind_CHINA},
#' \code{L232.SubsectorInterp_ind_CHINA}, \code{L232.StubTech_ind_CHINA}, \code{L232.StubTechInterp_ind_CHINA},
#' \code{L232.PerCapitaBased_ind_CHINA}, \code{L232.PriceElasticity_ind_CHINA}, \code{L232.IncomeElasticity_ind_gcam3_CHINA}.
#' The corresponding file in the original data system was \code{L232.industry_CHINA.R} (gcam-china level2).
#' @details Prepare level 2 industry sector files for CHINA.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by left_join mutate select semi_join summarise
#' @importFrom tidyr gather spread
#' @author YangLiu December 2019 / YangOu July 2023 / YangLiu September 2023
module_gcamchina_L232.industry_CHINA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "energy/A32.demand",
             FILE = "energy/A32.globaltech_eff",
             FILE = "energy/calibrated_techs",
            "L232.Supplysector_ind",
            "L232.FinalEnergyKeyword_ind",
            "L232.SubsectorLogit_ind",
            "L232.SubsectorShrwtFllt_ind",
            "L232.SubsectorInterp_ind",
            "L232.StubTech_ind",
            "L232.StubTechInterp_ind",
            "L232.PerCapitaBased_ind",
            "L232.PriceElasticity_ind",
            "L232.IncomeElasticity_ind_gcam3",
            "L1323.in_EJ_province_indnochp_F",
            "L1323.in_EJ_province_indfeed_F",
            "L132.in_EJ_province_indchp_F",
            "L2323.Supplysector_iron_steel",
            "L2324.Supplysector_Off_road",
            "L2325.Supplysector_chemical",
            "L2326.Supplysector_aluminum",
            "L2323.PerCapitaBased_iron_steel",
            "L2324.PerCapitaBased_Off_road",
            "L2325.PerCapitaBased_chemical",
            "L2326.PerCapitaBased_aluminum",
            "L238.SubsectorAll_tra",
            "L238.SubsectorAll_reg",
            "L238.Production_reg_imp",
            "L238.Production_tra",
            "L2323.BaseService_iron_steel"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L232.DeleteSupplysector_CHINAind",
             "L232.DeleteFinalDemand_CHINAind",
             "L232.DeleteDomSubsector_CHINAind",
             "L232.DeleteTraSubsector_CHINAind",
             "L232.Production_reg_imp_CHINA",
             "L232.BaseService_iron_steel_CHINA",
             "L232.StubTechCalInput_indenergy_CHINA",
             "L232.StubTechCalInput_indfeed_CHINA",
             "L232.StubTechProd_industry_CHINA",
             "L232.StubTechCoef_industry_CHINA",
             "L232.StubTechMarket_ind_CHINA",
             "L232.StubTechSecMarket_ind_CHINA",
             "L232.BaseService_ind_CHINA",
             "L232.DeleteSubsector_ind_CHINA",
             "L232.Supplysector_ind_CHINA",
             "L232.FinalEnergyKeyword_ind_CHINA",
             "L232.SubsectorLogit_ind_CHINA",
             "L232.SubsectorShrwtFllt_ind_CHINA",
             "L232.SubsectorInterp_ind_CHINA",
             "L232.StubTech_ind_CHINA",
             "L232.StubTechInterp_ind_CHINA",
             "L232.PerCapitaBased_ind_CHINA",
             "L232.PriceElasticity_ind_CHINA",
             "L232.IncomeElasticity_ind_gcam3_CHINA"))
  } else if(command == driver.MAKE) {

    # silence check package notes
    year <- value <- output_tot <- grid.region <- market.name <-
      calOutputValue <- calibrated.value <- calibration <- technology <-
      efficiency <- fuel <- minicam.energy.input <- object <-
      output_tot <- region <- secondary.output <- sector <- province <-
      subs.share.weight <- subsector <- supplysector <- value <- x <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = T)
    A32.demand <- get_data(all_data, "energy/A32.demand", strip_attributes = T)
    A32.globaltech_eff <- get_data(all_data, "energy/A32.globaltech_eff", strip_attributes = T)
    L1323.in_EJ_province_indnochp_F <- get_data(all_data, "L1323.in_EJ_province_indnochp_F", strip_attributes = T)
    L1323.in_EJ_province_indfeed_F <- get_data(all_data, "L1323.in_EJ_province_indfeed_F", strip_attributes = T)
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs", strip_attributes = T)
    L232.Supplysector_ind <- get_data(all_data, "L232.Supplysector_ind", strip_attributes = T)
    L232.FinalEnergyKeyword_ind <- get_data(all_data, "L232.FinalEnergyKeyword_ind", strip_attributes = T)
    L232.SubsectorLogit_ind <- get_data(all_data, "L232.SubsectorLogit_ind", strip_attributes = T)
    L232.SubsectorShrwtFllt_ind <- get_data(all_data, "L232.SubsectorShrwtFllt_ind", strip_attributes = T)
    L232.SubsectorInterp_ind <- get_data(all_data, "L232.SubsectorInterp_ind", strip_attributes = T)
    L232.StubTech_ind <- get_data(all_data, "L232.StubTech_ind", strip_attributes = T)
    L232.StubTechInterp_ind <- get_data(all_data, "L232.StubTechInterp_ind", strip_attributes = T)
    L232.PerCapitaBased_ind <- get_data(all_data, "L232.PerCapitaBased_ind", strip_attributes = T)
    L232.PriceElasticity_ind <- get_data(all_data, "L232.PriceElasticity_ind", strip_attributes = T)
    L232.IncomeElasticity_ind_gcam3 <- get_data(all_data, "L232.IncomeElasticity_ind_gcam3", strip_attributes = T)
    L132.in_EJ_province_indchp_F <- get_data(all_data, "L132.in_EJ_province_indchp_F", strip_attributes = T)
    L2323.Supplysector_iron_steel <- get_data(all_data, "L2323.Supplysector_iron_steel", strip_attributes = TRUE)
    L2324.Supplysector_Off_road <- get_data(all_data, "L2324.Supplysector_Off_road", strip_attributes = TRUE)
    L2325.Supplysector_chemical <- get_data(all_data, "L2325.Supplysector_chemical", strip_attributes = TRUE)
    L2326.Supplysector_aluminum <- get_data(all_data, "L2326.Supplysector_aluminum", strip_attributes = TRUE)
    L2323.PerCapitaBased_iron_steel <- get_data(all_data, "L2323.PerCapitaBased_iron_steel", strip_attributes = TRUE)
    L2324.PerCapitaBased_Off_road <- get_data(all_data, "L2324.PerCapitaBased_Off_road", strip_attributes = TRUE)
    L2325.PerCapitaBased_chemical <- get_data(all_data, "L2325.PerCapitaBased_chemical", strip_attributes = TRUE)
    L2326.PerCapitaBased_aluminum <- get_data(all_data, "L2326.PerCapitaBased_aluminum", strip_attributes = TRUE)
    L238.SubsectorAll_tra <- get_data(all_data, "L238.SubsectorAll_tra", strip_attributes = TRUE)
    L238.SubsectorAll_reg <- get_data(all_data, "L238.SubsectorAll_reg", strip_attributes = TRUE)
    L238.Production_reg_imp <- get_data(all_data, "L238.Production_reg_imp", strip_attributes = TRUE)
    L238.Production_tra <- get_data(all_data, "L238.Production_tra", strip_attributes = TRUE)
    L2323.BaseService_iron_steel <- get_data(all_data, "L2323.BaseService_iron_steel", strip_attributes = TRUE)

    # ===================================================
    # Data Processing

    # convert to long form
    A32.globaltech_eff %>%
      gather_years -> A32.globaltech_eff

    # delete industry sectors in the CHINA region (energy-final-demands and supplysectors)
    L232.Supplysector_ind %>%
      bind_rows(L2323.Supplysector_iron_steel,
                L2324.Supplysector_Off_road,
                L2325.Supplysector_chemical,
                L2326.Supplysector_aluminum) %>%
      mutate(region = region) %>% # strip attributes from object
      filter(region == gcamchina.REGION) %>%
      select(LEVEL2_DATA_NAMES[["DeleteSupplysector"]]) ->
      L232.DeleteSupplysector_CHINAind  ## OUTPUT

    # deleting energy final demand sectors in the full CHINA region")
    L232.PerCapitaBased_ind %>%
      bind_rows(L2324.PerCapitaBased_Off_road,
                L2325.PerCapitaBased_chemical,
                L2326.PerCapitaBased_aluminum) %>%
      mutate(region = region) %>% # strip attributes from object
      filter(region == gcamchina.REGION) %>%
      select(LEVEL2_DATA_NAMES[["DeleteFinalDemand"]]) ->
      L232.DeleteFinalDemand_CHINAind  ## OUTPUT

    # deleting traded iron and steel subsector in the full China region
    L238.SubsectorAll_tra %>%
      select(region,supplysector,subsector)%>%
      mutate(region = region) %>% # strip attributes from object
      filter(subsector == "China traded iron and steel") %>%
      select(LEVEL2_DATA_NAMES[["DeleteSubsector"]]) ->
      L232.DeleteTraSubsector_CHINAind  ## OUTPUT

    # deleting domestic iron and steel subsector in the full China region
    L238.SubsectorAll_reg %>%
      select(region,supplysector,subsector)%>%
      mutate(region = region) %>% # strip attributes from object
      filter(region == gcamchina.REGION, subsector == "domestic iron and steel") %>%
      select(LEVEL2_DATA_NAMES[["DeleteSubsector"]]) ->
      L232.DeleteDomSubsector_CHINAind  ## OUTPUT

    # calculate net iron and steel imports for the China
    L238.Production_reg_imp %>%
      filter(region == gcamchina.REGION) %>%
      left_join(L238.Production_tra %>%
                  filter(subsector =="China traded iron and steel") %>%
                  rename(Exports=calOutputValue)%>%
                  select(region,year,Exports) %>%
                  mutate(region = gcamchina.REGION), by = c("region", "year")) %>%
      mutate(calOutputValue=calOutputValue-Exports)%>%
      select(-Exports)-> L232.Production_reg_imp

    # Update base-year service for China region to match the above calculated net iron and steel imports
    L2323.BaseService_iron_steel %>%
      filter(region == gcamchina.REGION) %>%
      left_join(L232.Production_reg_imp %>%
                  select(region,year,calOutputValue) %>%
                  rename(Net_Imports=calOutputValue), by = c("region", "year"))%>%
      mutate(base.service=Net_Imports)%>%
      select(-Net_Imports)-> L232.BaseService_iron_steel

    # The industry_CHINA_processing function is used in place of a for loop in the old data sytem.
    # This function checks to see if the input data needs to be expanded to all provinces or used as
    # is.

    # industry_CHINA_processing: is a function that
    industry_CHINA_processing <- function(data) {

      # Subset the input data frame for the CHINA region. The subsetted data will be used
      # to check to see if the data frame needs to be processed, it's assumed that if the CHINA
      # is not found in the region column that regions have already been processed.

      check_CHINA <- filter(data, region == gcamchina.REGION)

      if(nrow(check_CHINA) == 0) {

        # This does not change the entries of the data frame but will strip the attributes
        # from the input data frame.
        new_data <- mutate(data, region = region)

      } else {

        # If the input data frame contains CHINA region information
        # then expand the input data to all provinces.

        data %>%
          filter(region == gcamchina.REGION) %>%
          write_to_all_provinces(names = names(data), gcamchina.PROVINCES_ALL) ->
          new_data

      }

      return(new_data)
    } # end of function

    L232.Supplysector_ind_CHINA <- industry_CHINA_processing(L232.Supplysector_ind)
    L232.FinalEnergyKeyword_ind_CHINA <- industry_CHINA_processing(L232.FinalEnergyKeyword_ind)
    L232.SubsectorLogit_ind_CHINA <- industry_CHINA_processing(L232.SubsectorLogit_ind)
    L232.SubsectorShrwtFllt_ind_CHINA <- industry_CHINA_processing(L232.SubsectorShrwtFllt_ind)
    L232.SubsectorInterp_ind_CHINA <- industry_CHINA_processing(L232.SubsectorInterp_ind)
    L232.StubTech_ind_CHINA <- industry_CHINA_processing(L232.StubTech_ind)
    L232.StubTechInterp_ind_CHINA <- industry_CHINA_processing(L232.StubTechInterp_ind)
    L232.PerCapitaBased_ind_CHINA <- industry_CHINA_processing(L232.PerCapitaBased_ind)
    L232.PriceElasticity_ind_CHINA <- industry_CHINA_processing(L232.PriceElasticity_ind)
    L232.IncomeElasticity_ind_gcam3_CHINA <- industry_CHINA_processing(L232.IncomeElasticity_ind_gcam3)

    # get calibrated input of industrial energy use technologies, including cogen
    L1323.in_EJ_province_indnochp_F %>%
      bind_rows(L132.in_EJ_province_indchp_F) %>%
      complete(nesting(province, sector, fuel), year = c(year, MODEL_BASE_YEARS)) %>%
      group_by(province, sector, fuel) %>%
      mutate(value = approx_fun(year, value,rule = 2)) %>%
      ungroup %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(region = province) %>%
      left_join_keep_first_only(calibrated_techs, by = c("sector", "fuel")) %>%
      rename(stub.technology = technology) ->
      L232.in_EJ_province_indenergy_F_Yh

    L232.in_EJ_province_indenergy_F_Yh %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], "value") %>%
      left_join_keep_first_only(select(A32.globaltech_eff, subsector, technology, minicam.energy.input),
                                by = c("subsector", "stub.technology" = "technology")) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      group_by(region, supplysector, subsector, year) %>%
      mutate(x = sum(calibrated.value),
             subs.share.weight = if_else(x > 0, 1, 0)) %>%
      ungroup %>%
      # ^^ sets up variable (x) for defining subsector shareweight
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) ->
      L232.StubTechCalInput_indenergy_CHINA  ## OUTPUT

    # get calibrated input of industrial feedstock technologies
    L1323.in_EJ_province_indfeed_F %>%
      complete(nesting(province, sector, fuel), year = c(year, MODEL_BASE_YEARS)) %>%
      group_by(province, sector, fuel) %>%
      mutate(value = approx_fun(year, value)) %>%
      ungroup %>% filter(year %in% MODEL_BASE_YEARS) %>%
      rename(region = province) %>%
      left_join_keep_first_only(calibrated_techs, by = c("sector", "fuel")) %>%
      select(-calibration, -secondary.output) %>%
      rename(stub.technology = technology) ->
      L232.in_EJ_province_indfeed_F_Yh

    L232.in_EJ_province_indfeed_F_Yh %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], "value") %>%
      left_join_keep_first_only(select(A32.globaltech_eff, subsector, technology, minicam.energy.input),
                                by = c("subsector", "stub.technology" = "technology")) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calibrated.value > 0, 1, 0)) %>%
      group_by(region, supplysector, subsector, year) %>%
      mutate(x = sum(calibrated.value)) %>%
      # ^^ sets up variable (x) for defining subsector shareweight
      mutate(subs.share.weight = if_else(x > 0, 1, 0)) %>%
      ungroup %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]]) ->
      L232.StubTechCalInput_indfeed_CHINA  ## OUTPUT

    # get industrial sector calibrated output
    A32.globaltech_eff %>%
      #First, need to interpolate industrial tech efficiencies
      complete(nesting(supplysector, subsector, technology, minicam.energy.input, secondary.output),
               year = c(year, MODEL_BASE_YEARS)) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input, secondary.output) %>%
      mutate(value = approx_fun(year, value)) %>% ungroup %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(efficiency = value) %>%
      mutate(efficiency = round(efficiency, energy.DIGITS_EFFICIENCY)) ->
      A32.globaltech_eff_interp

    L232.in_EJ_province_indenergy_F_Yh %>%
      bind_rows(L232.in_EJ_province_indfeed_F_Yh) %>%
      left_join_keep_first_only(select(A32.globaltech_eff_interp, supplysector, subsector, technology, year, efficiency),
                                by = c("supplysector", "subsector", "stub.technology" = "technology", "year")) %>%
      mutate(calOutputValue = round(value * efficiency, energy.DIGITS_CALOUTPUT)) ->
      L232.out_EJ_province_ind_serv_F_Yh
    # ^^ service output, by technology, for energy-use and feedstocks

    L232.out_EJ_province_ind_serv_F_Yh %>%
      group_by(region, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup %>%
      # ^^ aggregate to get output of industrial sector in each region
      mutate(supplysector = "other industry",
             subsector = "other industry",
             stub.technology = "other industry",
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L232.StubTechProd_industry_CHINA  ## OUTPUT

    # create the stub tech coefficients of industrial sector
    L232.out_EJ_province_ind_serv_F_Yh %>%
      group_by(region, supplysector, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>% ungroup %>%
      # ^^ aggregate service output by sector
      left_join_keep_first_only(L232.StubTechProd_industry_CHINA %>%
                                  rename(output_tot = calOutputValue) %>% select(region, year, output_tot),
                                by = c("region", "year")) %>%
      mutate(coefficient = round(calOutputValue / output_tot, energy.DIGITS_COEFFICIENT)) %>%
      # ^^ get coefficient
      rename(minicam.energy.input = supplysector) %>%
      mutate(supplysector = "other industry",
             subsector = "other industry",
             stub.technology = "other industry",
             market.name = region) ->
      L232.StubTechCoef_industry_CHINA_base
    # ^^ covers only base years

    L232.StubTechCoef_industry_CHINA_base %>%
      filter(year == max(MODEL_BASE_YEARS)) %>% select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) ->
      L232.StubTechCoef_industry_CHINA_fut
    # ^^ future years copied from final base year
    # note: this is not typical, but is suitable here as no energy:feedstock evolution in the industrial...
    # ... sector is expected for CHINA

    bind_rows(L232.StubTechCoef_industry_CHINA_base, L232.StubTechCoef_industry_CHINA_fut) %>%
      select(LEVEL2_DATA_NAMES$StubTechCoef) ->
      L232.StubTechCoef_industry_CHINA  ## OUTPUT

    # Get markets for fuels consumed by the province industrial sectors
    L232.StubTech_ind %>%
      filter(region == gcamchina.REGION) %>%
      select(-region) %>%
      write_to_all_provinces(names = c(names(L232.StubTech_ind), "region"), gcamchina.PROVINCES_ALL) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join_keep_first_only(A32.globaltech_eff %>%
                                  select(supplysector, subsector, technology, minicam.energy.input),
                                by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      filter(is.na(minicam.energy.input) == FALSE) %>%
      #The table of all stub technologies includes the generic industrial technology, which doesn't apply here.
      #Only setting markets here for the ones that consume fuels.
      mutate(market.name = gcamchina.REGION) %>%
      select(LEVEL2_DATA_NAMES[["StubTechMarket"]]) %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamchina.REGIONAL_FUEL_MARKETS,
                                   region, market.name)) %>%
      mutate(market.name = if_else(minicam.energy.input == "district heat",
                                   region, market.name)) %>%
      #NOTE: electricity is consumed from province markets
      mutate(market.name = if_else(minicam.energy.input %in% gcamchina.PROVINCE_FUEL_MARKETS,
                                   region, market.name)) ->
      L232.StubTechMarket_ind_CHINA  ## OUTPUT

    # markets for the cogenerated electricity (secondary output)
    A32.globaltech_eff %>%
      filter(is.na(secondary.output) == FALSE) %>%
      select(supplysector, subsector, technology) ->
      L232.chp_techs

    L232.StubTechMarket_ind_CHINA %>%
      # ^^ electricity is consumed from province markets
      semi_join(L232.chp_techs, by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      # ^^ filters for rows contained in L232.chp_techs
      mutate(secondary.output = "electricity") %>%
      select(LEVEL2_DATA_NAMES[["StubTechYr"]], "secondary.output", "market.name") %>%
      mutate(market.name = region) %>%
      # ^^ over-ride regional market names
      left_join_error_no_match(province_names_mappings %>%
                                 select(province, grid.region),
                               by = c("region" = "province")) %>%
      mutate(market.name = grid.region) %>%
      select(-grid.region) ->
      L232.StubTechSecMarket_ind_CHINA  ## OUTPUT

    # base-year service output of industry final demand
    L232.StubTechProd_industry_CHINA %>%
      select(region, year, calOutputValue) %>%
      rename(base.service = calOutputValue) %>%
      mutate(energy.final.demand = A32.demand$energy.final.demand) ->
      L232.BaseService_ind_CHINA  # base service is equal to the output of the industry supplysector

    L232.StubTechMarket_ind_CHINA %>%
      filter(subsector != "hydrogen"  ) %>%
      anti_join(L232.StubTechCalInput_indenergy_CHINA[, LEVEL2_DATA_NAMES[["Subsector"]]] %>%
                  full_join(L232.StubTechCalInput_indfeed_CHINA[, LEVEL2_DATA_NAMES[["Subsector"]]],
                            by = c("region", "supplysector", "subsector")),
                by = c("region", "supplysector", "subsector")) %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      unique() ->
      L232.DeleteSubsector_ind_CHINA

    # ===================================================
    # Produce outputs

    L232.DeleteSupplysector_CHINAind %>%
      add_title("CHINA industry supply sectors") %>%
      add_units("NA") %>%
      add_comments("Generated by deselecting industry sectors from input") %>%
      add_legacy_name("L232.DeleteSupplysector_CHINAind") %>%
      add_precursors("L232.Supplysector_ind",
                     "L2323.Supplysector_iron_steel",
                     "L2324.Supplysector_Off_road",
                     "L2325.Supplysector_chemical",
                     "L2326.Supplysector_aluminum") ->
      L232.DeleteSupplysector_CHINAind

    L232.DeleteFinalDemand_CHINAind %>%
      add_title("CHINA final energy demand table for industry") %>%
      add_units("NA") %>%
      add_comments("Generated by deselecting final demand sectors") %>%
      add_legacy_name("L232.DeleteFinalDemand_CHINAind") %>%
      add_precursors("L232.PerCapitaBased_ind",
                     "L2323.PerCapitaBased_iron_steel",
                     "L2324.PerCapitaBased_Off_road",
                     "L2325.PerCapitaBased_chemical",
                     "L2326.PerCapitaBased_aluminum") ->
      L232.DeleteFinalDemand_CHINAind

    L232.DeleteDomSubsector_CHINAind %>%
      add_title("China domestic iron and steel table for industry") %>%
      add_units("NA") %>%
      add_comments("Generated by deselecting domestic iron and steel subsector in the global China region") %>%
      add_legacy_name("L232.DeleteDomSubsector_USAind") %>%
      add_precursors("L238.SubsectorAll_reg") ->
      L232.DeleteDomSubsector_CHINAind

    L232.DeleteTraSubsector_CHINAind %>%
      add_title("China traded iron and steel table for industry") %>%
      add_units("NA") %>%
      add_comments("Generated by deselecting traded iron and steel subsector in the global China region") %>%
      add_legacy_name("L232.DeleteTraSubsector_USAind") %>%
      add_precursors("L238.SubsectorAll_tra") ->
      L232.DeleteTraSubsector_CHINAind

    L232.Production_reg_imp %>%
      add_title("China net imports") %>%
      add_units("NA") %>%
      add_comments("China net imports add-on to allow iron and steel trade work for other global regions in GCAM China") %>%
      add_legacy_name("L232.DeleteTraSubsector_USAind") %>%
      add_precursors("L238.Production_reg_imp", "L238.Production_tra") ->
      L232.Production_reg_imp_CHINA

    L232.BaseService_iron_steel %>%
      add_title("China regional iron and steel base-service by year") %>%
      add_units("NA") %>%
      add_comments("The regional iron and steel base service is re-calibrated to China net imports for GCAM China") %>%
      add_legacy_name("L232.BaseService_iron_steel") %>%
      add_precursors("L2323.BaseService_iron_steel") ->
      L232.BaseService_iron_steel_CHINA

    L232.StubTechCalInput_indenergy_CHINA %>%
      add_title("calibrated input of industrial energy use technologies (including cogen)") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights generated from calibrated values") %>%
      add_legacy_name("L232.StubTechCalInput_indenergy_CHINA") %>%
      add_precursors("L1323.in_EJ_province_indnochp_F",
                     "L132.in_EJ_province_indchp_F",
                     "energy/calibrated_techs",
                     "energy/A32.globaltech_eff") ->
      L232.StubTechCalInput_indenergy_CHINA

    L232.StubTechCalInput_indfeed_CHINA %>%
      add_title("Calibrated input of industrial feedstock technologies") %>%
      add_units("Unitless") %>%
      add_comments("Shareweights generated from calibrated values") %>%
      add_legacy_name("L232.StubTechCalInput_indfeed_CHINA") %>%
      add_precursors("L1323.in_EJ_province_indfeed_F",
                     "energy/calibrated_techs",
                     "energy/A32.globaltech_eff") ->
      L232.StubTechCalInput_indfeed_CHINA

    L232.StubTechProd_industry_CHINA %>%
      add_title("industrial sector output") %>%
      add_units("Unitless") %>%
      add_comments("Service output aggregated to industrial sector for each region") %>%
      add_legacy_name("L232.StubTechProd_industry_CHINA") %>%
      add_precursors("energy/A32.globaltech_eff",
                     "L1323.in_EJ_province_indnochp_F",
                     "L1323.in_EJ_province_indfeed_F") ->
      L232.StubTechProd_industry_CHINA

    L232.StubTechCoef_industry_CHINA %>%
      add_title("industrial sector calibrated output") %>%
      add_units("Unitless") %>%
      add_comments("Generated by bind base and future year coefficients") %>%
      add_legacy_name("L232.StubTechCoef_industry_CHINA") %>%
      add_precursors("energy/A32.globaltech_eff",
                     "L1323.in_EJ_province_indnochp_F",
                     "L1323.in_EJ_province_indfeed_F") ->
      L232.StubTechCoef_industry_CHINA

    L232.StubTechMarket_ind_CHINA %>%
      add_title("Markets for the fuels consumed by the province industrial sectors") %>%
      add_units("NA") %>%
      add_comments("") %>%
      add_legacy_name("L232.StubTechMarket_ind_CHINA") %>%
      add_precursors("L232.StubTech_ind",
                     "energy/A32.globaltech_eff",
                     "gcam-china/province_names_mappings") ->
      L232.StubTechMarket_ind_CHINA

    L232.StubTechSecMarket_ind_CHINA %>%
      add_title("markets for the cogenerated electricity (secondary output)") %>%
      add_units("NA") %>%
      add_comments("derived from L232.StubTechMarket_ind_CHINA") %>%
      add_legacy_name("L232.StubTechSecMarket_ind_CHINA") %>%
      add_precursors("L232.StubTech_ind",
                     "energy/A32.globaltech_eff",
                     "gcam-china/province_names_mappings") ->
      L232.StubTechSecMarket_ind_CHINA

    L232.BaseService_ind_CHINA %>%
      add_title("base-year service output of industry final demand") %>%
      add_units("NA") %>%
      add_comments("base service is equal to the output of the industry supplysector") %>%
      add_legacy_name("L232.BaseService_ind_CHINA") %>%
      add_precursors("energy/A32.globaltech_eff",
                     "L1323.in_EJ_province_indnochp_F",
                     "L1323.in_EJ_province_indfeed_F",
                     "energy/A32.demand") ->
      L232.BaseService_ind_CHINA

    L232.DeleteSubsector_ind_CHINA %>%
      add_title("Delete subsectors that did not have any calibration data") %>%
      add_units("NA") %>%
      add_comments("Delete subsectors that did not have any calibration data") %>%
      add_legacy_name("L232.DeleteSubsector_ind_CHINA") %>%
      add_precursors("L232.StubTechMarket_ind_CHINA") ->
      L232.DeleteSubsector_ind_CHINA

    L232.Supplysector_ind_CHINA %>%
      add_title("Supply sector information for industry sector") %>%
      add_units("NA") %>%
      add_comments("For industry sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A32.sector is expanded for GCAM-CHINA") %>%
      add_legacy_name("L232.Supplysector_ind_CHINA") %>%
      add_precursors("L232.Supplysector_ind") ->
      L232.Supplysector_ind_CHINA

    L232.FinalEnergyKeyword_ind_CHINA %>%
      add_title("Supply sector keywords for industry sector") %>%
      add_units("NA") %>%
      add_comments("Set supply sector keywords for industry sector for all GCAM-CHINA regions") %>%
      add_legacy_name("L232.FinalEnergyKeyword_ind_CHINA") %>%
      add_precursors("L232.FinalEnergyKeyword_ind") ->
      L232.FinalEnergyKeyword_ind_CHINA

    L232.SubsectorLogit_ind_CHINA %>%
      add_title("Subsector logit exponents of industry sector") %>%
      add_units("Unitless") %>%
      add_comments("For industry sector, the subsector logit exponents from A32.subsector_logit are expanded into all GCAM-CHINA regions with non-existent heat subsectors removed") %>%
      add_legacy_name("L232.SubsectorLogit_ind_CHINA") %>%
      add_precursors("L232.SubsectorLogit_ind") ->
      L232.SubsectorLogit_ind_CHINA

    L232.SubsectorShrwtFllt_ind_CHINA %>%
      add_title("Subsector shareweights of industry sector") %>%
      add_units("Unitless") %>%
      add_comments("For industry sector, the subsector shareweights from A32.subsector_shrwt are expanded into all GCAM-CHINA regions with non-existent heat technologies") %>%
      add_legacy_name("L232.SubsectorShrwtFllt_ind_CHINA") %>%
      add_precursors("L232.SubsectorShrwtFllt_ind") ->
      L232.SubsectorShrwtFllt_ind_CHINA

    L232.SubsectorInterp_ind_CHINA %>%
      add_title("Subsector shareweight interpolation of industry sector") %>%
      add_units("NA") %>%
      add_comments("For industry sector, the subsector shareweight interpolation function infromation from A32.subsector_interp is expanded into all GCAM-CHINA regions with non-existent heat technologies removed") %>%
      add_legacy_name("L232.SubsectorInterp_ind_CHINA") %>%
      add_precursors("L232.SubsectorInterp_ind") ->
      L232.SubsectorInterp_ind_CHINA

    L232.StubTech_ind_CHINA %>%
      add_title("Identification of stub technologies of industrial sector") %>%
      add_units("NA") %>%
      add_comments("For industry sector, the stub technologies from A32.globaltech_shrwt are expanded into all GCAM-CHINA regions with non-existent heat technologies removed") %>%
      add_legacy_name("L232.StubTech_ind_CHINA") %>%
      add_precursors("L232.StubTech_ind") ->
      L232.StubTech_ind_CHINA

    L232.StubTechInterp_ind_CHINA %>%
      add_title("Shareweight interpolation of global industrial sector technologies") %>%
      add_units("NA") %>%
      add_comments("For industry sector, the interpolation function from A32.globaltech_interp are expanded into all GCAM regions") %>%
      add_legacy_name("L232.StubTechInterp_ind_CHINA") %>%
      add_precursors("L232.StubTechInterp_ind") ->
      L232.StubTechInterp_ind_CHINA

    L232.PerCapitaBased_ind_CHINA %>%
      add_title("Per-capita based flag for industry final demand") %>%
      add_units("NA") %>%
      add_comments("Extracted per-capita based flag for industry final demand from A32.demand") %>%
      add_legacy_name("L232.PerCapitaBased_ind_CHINA") %>%
      add_precursors("L232.PerCapitaBased_ind") ->
      L232.PerCapitaBased_ind_CHINA

    L232.PriceElasticity_ind_CHINA %>%
      add_title("Price elasticity of industry final demand") %>%
      add_units("Unitless") %>%
      add_comments("Extracted price elasticity of industry final demand from A32.demand") %>%
      add_comments("Price elasticities are only applied to future periods. Application in base years will cause solution failure") %>%
      add_legacy_name("L232.PriceElasticity_ind_CHINA") %>%
      add_precursors("L232.PriceElasticity_ind") ->
      L232.PriceElasticity_ind_CHINA

    L232.IncomeElasticity_ind_gcam3_CHINA %>%
      add_title("Income elasticity of industry - GCAM3") %>%
      add_units("Unitless") %>%
      add_comments("First calculate industrial output as the base-year industrial output times the GDP ratio raised to the income elasticity") %>%
      add_comments("Then back out the appropriate income elasticities from industrial output") %>%
      add_comments("Note lower income elasticities for SSP1 are hard-coded.") %>%
      add_legacy_name("L232.IncomeElasticity_ind_gcam3_CHINA") %>%
      add_precursors("L232.IncomeElasticity_ind_gcam3") ->
      L232.IncomeElasticity_ind_gcam3_CHINA


    return_data(L232.DeleteSupplysector_CHINAind,
                L232.DeleteFinalDemand_CHINAind,
                L232.DeleteDomSubsector_CHINAind,
                L232.DeleteTraSubsector_CHINAind,
                L232.Production_reg_imp_CHINA,
                L232.BaseService_iron_steel_CHINA,
                L232.StubTechCalInput_indenergy_CHINA,
                L232.StubTechCalInput_indfeed_CHINA,
                L232.StubTechProd_industry_CHINA,
                L232.StubTechCoef_industry_CHINA,
                L232.StubTechMarket_ind_CHINA,
                L232.StubTechSecMarket_ind_CHINA,
                L232.BaseService_ind_CHINA,
                L232.DeleteSubsector_ind_CHINA,
                L232.Supplysector_ind_CHINA,
                L232.FinalEnergyKeyword_ind_CHINA,
                L232.SubsectorLogit_ind_CHINA,
                L232.SubsectorShrwtFllt_ind_CHINA,
                L232.SubsectorInterp_ind_CHINA,
                L232.StubTech_ind_CHINA,
                L232.StubTechInterp_ind_CHINA,
                L232.PerCapitaBased_ind_CHINA,
                L232.PriceElasticity_ind_CHINA,
                L232.IncomeElasticity_ind_gcam3_CHINA)
  } else {
    stop("Unknown command")
  }
}
