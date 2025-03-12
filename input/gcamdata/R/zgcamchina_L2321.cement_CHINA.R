# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L2321.cement
#'
#' Make the logit and input tables for the cement sector in gcam-china
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2321.DeleteSupplysector_CHINAcement}, \code{L2321.DeleteFinalDemand_CHINAcement}, \code{L2321.StubTechProd_cement_CHINA},
#' \code{L2321.StubTechCoef_cement_CHINA}, \code{L2321.StubTechCalInput_cement_heat_CHINA}, \code{L2321.StubTechMarket_cement_CHINA}, \code{L2321.BaseService_cement_CHINA},
#' \code{L2321.Supplysector_cement_CHINA}, \code{L2321.FinalEnergyKeyword_cement_CHINA}, \code{L2321.SubsectorLogit_cement_CHINA},
#' \code{L2321.SubsectorShrwtFllt_cement_CHINA}, \code{L2321.StubTech_cement_CHINA}, \code{L2321.PerCapitaBased_cement_CHINA},
#'  \code{L2321.PriceElasticity_cement_CHINA}, \code{L2321.IncomeElasticity_cement_gcam3_CHINA} .
#'  The corresponding file in the original data system was \code{L2321.cement_CHINA.R} (gcam-china level2).
#' @details Make the logit and input tables for the cement sector in gcam-china
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange distinct filter if_else group_by left_join mutate select
#' @importFrom tidyr gather spread
#' @author YangLiu  December 2019
module_gcamchina_L2321.cement <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A321.demand",
             FILE = "energy/A321.globaltech_coef",
             "L2321.Supplysector_cement",
             "L2321.FinalEnergyKeyword_cement",
             "L2321.SubsectorLogit_cement",
             "L2321.SubsectorShrwtFllt_cement",
             "L2321.SubsectorInterp_cement",
             "L2321.StubTech_cement",
             "L2321.PerCapitaBased_cement",
             "L2321.PriceElasticity_cement",
             "L2321.IncomeElasticity_cement_gcam3",
             "L1321.in_EJ_province_cement_F_Y",
             "L1321.IO_GJkg_province_cement_F_Yh",
             "L1321.out_Mt_province_cement_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2321.DeleteSupplysector_CHINAcement",
             "L2321.DeleteFinalDemand_CHINAcement",
             "L2321.StubTechProd_cement_CHINA",
             "L2321.StubTechCoef_cement_CHINA",
             "L2321.StubTechCalInput_cement_heat_CHINA",
             "L2321.StubTechMarket_cement_CHINA",
             "L2321.BaseService_cement_CHINA",
             "L2321.Supplysector_cement_CHINA",
             "L2321.FinalEnergyKeyword_cement_CHINA",
             "L2321.SubsectorLogit_cement_CHINA",
             "L2321.SubsectorShrwtFllt_cement_CHINA",
             "L2321.SubsectorInterp_cement_CHINA",
             "L2321.StubTech_cement_CHINA",
             "L2321.PerCapitaBased_cement_CHINA",
             "L2321.PriceElasticity_cement_CHINA",
             "L2321.IncomeElasticity_cement_gcam3_CHINA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = T)
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs", strip_attributes = T)
    A321.demand <- get_data(all_data, "energy/A321.demand", strip_attributes = T)
    A321.globaltech_coef <- get_data(all_data, "energy/A321.globaltech_coef", strip_attributes = T)

    L2321.Supplysector_cement <- get_data(all_data, "L2321.Supplysector_cement", strip_attributes = T)
    L2321.FinalEnergyKeyword_cement <- get_data(all_data, "L2321.FinalEnergyKeyword_cement", strip_attributes = T)
    L2321.SubsectorLogit_cement <- get_data(all_data, "L2321.SubsectorLogit_cement", strip_attributes = T)
    L2321.SubsectorShrwtFllt_cement <- get_data(all_data, "L2321.SubsectorShrwtFllt_cement", strip_attributes = T)
    L2321.SubsectorInterp_cement <- get_data(all_data, "L2321.SubsectorInterp_cement", strip_attributes = T)
    L2321.StubTech_cement <- get_data(all_data, "L2321.StubTech_cement", strip_attributes = T)
    L2321.PerCapitaBased_cement <- get_data(all_data, "L2321.PerCapitaBased_cement", strip_attributes = T)
    L2321.PriceElasticity_cement <- get_data(all_data, "L2321.PriceElasticity_cement", strip_attributes = T)
    L2321.IncomeElasticity_cement_gcam3 <- get_data(all_data, "L2321.IncomeElasticity_cement_gcam3", strip_attributes = T)
    L1321.in_EJ_province_cement_F_Y <- get_data(all_data, "L1321.in_EJ_province_cement_F_Y", strip_attributes = T)
    L1321.IO_GJkg_province_cement_F_Yh <- get_data(all_data, "L1321.IO_GJkg_province_cement_F_Yh", strip_attributes = T)
    L1321.out_Mt_province_cement_Yh <- get_data(all_data, "L1321.out_Mt_province_cement_Yh", strip_attributes = T)

    # Silence package checks
    province <- region <- supplysector <- energy.final.demand <- region <- year <-
      value <- calibration <- sector <- subsector <- technology <- calOutputValue <-
      subs.share.weight <- share.weight.year <- fuel <- minicam.energy.input <-
      coefficient <- market.name <- grid.region <- stub.technology <- calibrated.value <-
      tech.share.weight <- object <- NULL

    # ===================================================
    # Make the logit and input tables for with creating cement sectors in cement
    # producing provinces.

    # Not all provinces produce cement so save a vector of cement producing provinces bases on census data.
    # This vector will be used to create cement sectors in only the cement producing provinces.
    L1321.out_Mt_province_cement_Yh %>%
      select(province) %>%
      unique ->
      cement_provinces


    # In order to create province cement sectors on the province level for gcam-CHINA the
    # cement sector will need to be removed from the CHINA region in the supply sector
    # and enery-final-demands input tables
    #
    # Create a table that will be used remove the cement supply sector input table.
    L2321.Supplysector_cement %>%
      filter(region == gcamchina.REGION) %>%
      select(region, supplysector) %>%
      # Mutate to remove attributes.
      mutate(region = region) ->
      L2321.DeleteSupplysector_CHINAcement

    # Create the table that will be used to remove the cement sector information from the
    # energy.final.demand input table.
    L2321.PerCapitaBased_cement %>%
      filter(region == gcamchina.REGION) %>%
      select(region, energy.final.demand) %>%
      # Mutate to remove attributes.
      mutate(region = region) ->
      L2321.DeleteFinalDemand_CHINAcement


    # The cement_CHINA_processing function replaces the "identical processing for loop"
    # in the old data system. Function inputs include an input data frame to be
    # checked and processed if deemed necessary and a list of the cement producing
    # provinces.

    cement_CHINA_processing <- function(data, cement_provinces) {

      # Subset the input data frame for the CHINA region. The subsetted data will be used
      # to check to see if the data frame needs to be processed, it's assumed that if the CHINA
      # is not found in the region column that regions have already been processed.

      check_df <- filter(data, region == gcamchina.REGION)

      if(nrow(check_df) == 0) {

        # This does not change the entries of the data frame but will strip the attributes
        # from the input data frame.
        new_data <- mutate(data, region = region)

      } else {

        # If the input data frame contains CHINA region information
        # then expand the input data to all cement producing provinces.

        data %>%
          filter(region == gcamchina.REGION) %>%
          write_to_all_provinces(names = names(data), gcamchina.PROVINCES_ALL) %>%
          filter(region %in% cement_provinces[["province"]]) ->
          new_data

      }

      return(new_data)
    } # end of function



    # Use the cement_CHINA_processing function to check and or process the following data frames so that
    # all of the output data frames contain information for all cement producing provinces.
    L2321.Supplysector_cement_CHINA <- cement_CHINA_processing(L2321.Supplysector_cement, cement_provinces)
    L2321.FinalEnergyKeyword_cement_CHINA <- cement_CHINA_processing(L2321.FinalEnergyKeyword_cement, cement_provinces)
    L2321.SubsectorLogit_cement_CHINA <- cement_CHINA_processing(L2321.SubsectorLogit_cement, cement_provinces)
    L2321.SubsectorShrwtFllt_cement_CHINA <- cement_CHINA_processing(L2321.SubsectorShrwtFllt_cement, cement_provinces)
    L2321.SubsectorInterp_cement_CHINA <- cement_CHINA_processing(L2321.SubsectorInterp_cement, cement_provinces)
    L2321.StubTech_cement_CHINA <- cement_CHINA_processing(L2321.StubTech_cement, cement_provinces)
    L2321.PerCapitaBased_cement_CHINA <- cement_CHINA_processing(L2321.PerCapitaBased_cement, cement_provinces)
    L2321.PriceElasticity_cement_CHINA <- cement_CHINA_processing(L2321.PriceElasticity_cement, cement_provinces)
    L2321.IncomeElasticity_cement_gcam3_CHINA <- cement_CHINA_processing(L2321.IncomeElasticity_cement_gcam3, cement_provinces)


    # Create stub-technology calibrated cement production input table.
    #
    # Start by sub setting the cement production by province / historical year
    # for model base years and rounding to the appropriate number of digits.
    L1321.out_Mt_province_cement_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = signif(value, energy.DIGITS_CALOUTPUT),
             region = province ) ->
      L2321.StubTechProd_cement_CHINA

    # Subset the calibrated intermediate sectors and fuels to supplysector / subsector / technology
    # mapping file for unique sector / calibration / supplysector/ subsector / technology combinations.
    # This tibble will be used to add cement sector information add to the province cement production
    # input table.
    calibrated_techs %>%
      # We are only interested in the technology IDs where calibration = output.
      filter(calibration == "output") %>%
      select(sector, calibration, supplysector, subsector, technology) %>%
      distinct ->
      calibrated_techs_cement_sector_info

    # Combine the cement sector information found above and the stub-technology calibrated
    # cement production into a single data frame.
    L2321.StubTechProd_cement_CHINA %>%
      left_join_error_no_match(calibrated_techs_cement_sector_info, by = "sector") %>%
      select(province, sector, calOutputValue, year, region, supplysector, subsector, technology) ->
      L2321.StubTechProd_cement_CHINA

    # Add share weight information to the province cement production data frame and format.
    L2321.StubTechProd_cement_CHINA %>%
      mutate(stub.technology = technology,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(region, supplysector, subsector, stub.technology, year, calOutputValue,
             share.weight.year, subs.share.weight, tech.share.weight) ->
      L2321.StubTechProd_cement_CHINA


    # Create the coefficients of cement production technologies input table.
    #
    # Start by creating a data frame of unique sector, fuel, supplysector, subsector,
    # technology, and minicam.energy.input combinations. This data frame will be used to
    # add sector information to input-ouput coefficients for province cement production.
    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct ->
      cement_production_technologies

    # Add cement sector information to the data frame of input-output coefficients of
    # cement production by province.
    L1321.IO_GJkg_province_cement_F_Yh %>%
      left_join_error_no_match(cement_production_technologies, by = c("sector", "fuel")) %>%
      select(province,fuel, sector, year, value, supplysector, subsector, technology, minicam.energy.input) ->
      L2321.IO_GJkg_province_cement_F_Yh

    # Interpolate cement production default coefficients to future years.
    #
    # First change format of the the production default coefficients data frame from wide to long.
    A321.globaltech_coef_long <- gather_years(A321.globaltech_coef)

    # Then linearly interpolate the default coefficients for future years. In the next step these
    # values will be added to the province input-output coefficients data frame.
    A321.globaltech_coef_long %>%
      complete(nesting(supplysector, subsector, minicam.energy.input, technology), year = c(year, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, minicam.energy.input, technology, year) %>%
      group_by(supplysector, subsector, minicam.energy.input, technology) %>%
      mutate(value = approx_fun(year, value), value = signif(value, energy.DIGITS_COEFFICIENT)) %>%
      ungroup ->
      L2321.globaltech_coef

    # Subset the global technology coefficients and format the data frame to join with the
    # province input-output coefficients.
    L2321.globaltech_coef %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      spread(year, value) ->
      L2321.globaltech_coef_yfut

    # Combine the future global technology coefficients with the province energy input-output
    # coefficients by supplysector / subsector / technology / minicam.energy.input combinations.
    L2321.IO_GJkg_province_cement_F_Yh %>%
      spread(year, value) %>%
      left_join_error_no_match(L2321.globaltech_coef_yfut,
                               by = c("supplysector", "subsector", "technology", "minicam.energy.input")) ->
      IO_and_globaltech

    # Format the the data frame and round the number of digits.
    IO_and_globaltech %>%
      gather_years %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(coefficient = signif(value, energy.DIGITS_COEFFICIENT)) %>%
      select(-value) ->
      L2321.IO_GJkg_province_cement_F_Yh_complete

    # Save lists of unique the minicam.energy.input from the complete
    # province input-output coefficients data frame to add to the province
    # stub technology data frame.
    L2321.IO_GJkg_province_cement_F_Yh_complete %>%
      select(minicam.energy.input) %>%
      unique ->
      minicam_to_add

    # Subset the cement stub-technology coefficient data frame for supply sectors in the
    # input-output data frame, add model years and minicam information in preparation
    # for the left join in the next step.
    L2321.StubTech_cement_CHINA  %>%
      filter(supplysector %in% L2321.IO_GJkg_province_cement_F_Yh_complete$supplysector) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(minicam_to_add) ->
      L2321.StubTechCoef_cement_CHINA

    # Join the input-output coefficients data frame with the  Cement stub technologies data frame by
    # region / supplysector/ minicam.energy.input/ year.
    L2321.StubTechCoef_cement_CHINA %>%
      left_join(L2321.IO_GJkg_province_cement_F_Yh_complete %>%
                  select(coefficient, region = province, supplysector, minicam.energy.input, year),
                by =c("region", "supplysector", "minicam.energy.input", "year")) ->
      L2321.StubTechCoef_cement_CHINA

    # Add market information: default is China replace for fuels with region level markets.
    # Process heat cement and limestone are also represented at the province level (names referenced from existing objects).
    L2321.StubTechCoef_cement_CHINA %>%
      left_join(province_names_mappings %>%
                  select(region = province, grid.region),
                by = "region") %>%
      mutate(market.name = gcamchina.REGION,
             market.name = if_else(minicam.energy.input %in% gcamchina.PROVINCE_FUEL_MARKETS,
                                   region, market.name),
             market.name = if_else(minicam.energy.input %in% gcamchina.REGIONAL_FUEL_MARKETS,
                                   grid.region, market.name)) %>%
      select(-grid.region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L2321.StubTechCoef_cement_CHINA

    # Create the calibrated input of fuel consumption for producing heat input table.
    #
    # Start by subsetting the energy inputs to cement production by province for model base years,
    # rounding to the correct xml digit and adding a region column.
    L1321.in_EJ_province_cement_F_Y %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calibrated.value = signif(value, gcamchina.DIGITS_CALOUTPUT), region = province) %>%
      select(-value)  ->
      L2321.StubTechCalInput_cement_heat_CHINA

    # Add supplysector / subsector / technology / minicam.energy.input information
    # from the calibrated technology mapping file to the energy inputs to cement production
    # data frame.
    L2321.StubTechCalInput_cement_heat_CHINA %>%
      left_join_error_no_match(calibrated_techs %>%
                                 select(sector, fuel, supplysector, subsector, technology, minicam.energy.input),
                               by = c("sector", "fuel")) ->
      L2321.StubTechCalInput_cement_heat_CHINA

    # Since this table should only contain the technologies for producing heat, remove
    # electricity inputs to the cement production technology from the stub technology coefficients by
    # filtering for supply sectors NOT included in the L2321.StubTechCoef_cement_CHINA data frame which
    # contains electricity inputs.
    L2321.StubTechCalInput_cement_heat_CHINA %>%
      filter(!supplysector %in% L2321.StubTechCoef_cement_CHINA$supplysector) ->
      L2321.StubTechCalInput_cement_heat_CHINA_NOelectricity

    # Add region and share weight information, format.
    L2321.StubTechCalInput_cement_heat_CHINA_NOelectricity %>%
      mutate(region = province,
             stub.technology = technology,
             share.weight.year = year,
             subs.share.weight = if_else(calibrated.value > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(region, supplysector, subsector, stub.technology, year, minicam.energy.input,
             calibrated.value, share.weight.year, subs.share.weight, tech.share.weight) ->
      L2321.StubTechCalInput_cement_heat_CHINA


    # Create the input table with the market names  for the fuels consumed for heat by the province cement sectors
    #
    # Add model years to the stub technology coefficients for the cement sector. Then remove the
    # cement supplysector leaving only the process heat supplysector.
    L2321.StubTech_cement_CHINA %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      filter(!supplysector %in% L2321.StubTechCoef_cement_CHINA$supplysector) ->
      L2321.StubTechMarket_cement_CHINA

    # Use the calibrated technology mapping data frame to add minicam.energy.input information to
    # the process heat supplysector data frame.
    L2321.StubTechMarket_cement_CHINA %>%
      # Use left join here to pass time shift test.
      left_join(A321.globaltech_coef %>%
                  select(supplysector, subsector, stub.technology = technology,
                         minicam.energy.input),
                by = c("supplysector", "subsector", "stub.technology")) ->
      L2321.StubTechMarket_cement_CHINA

    # Fuels are from the CHINA markets, except for regional fuel markets
    L2321.StubTechMarket_cement_CHINA %>%
      mutate(market.name = gcamchina.REGION) %>%
      select(region, supplysector, subsector, stub.technology, year, minicam.energy.input, market.name) %>%
      # replace market name with the  region name if the minicam.energy.input is
      # considered a regional fuel market, and with the province name if it's a province market
      mutate(market.name = if_else(minicam.energy.input %in% gcamchina.REGIONAL_FUEL_MARKETS,
                                   region, market.name),
             market.name = if_else(minicam.energy.input %in% gcamchina.PROVINCE_FUEL_MARKETS,
                                   region, market.name)) ->
      L2321.StubTechMarket_cement_CHINA


    # Create the base-year service output for cement final demand input table.
    #
    # Since base service is equal to the output of the cement supplysector use
    # the coefficients from the stub technology production data frame and add
    # energy.final.demand form cement final demand perCapitaBased and price elasticity
    # assumption file.
    L2321.StubTechProd_cement_CHINA %>%
      mutate(energy.final.demand = A321.demand$energy.final.demand) %>%
      select(region, energy.final.demand, year, base.service = calOutputValue) ->
      L2321.BaseService_cement_CHINA

    # ===================================================
    # Produce outputs
    L2321.DeleteSupplysector_CHINAcement %>%
      add_title("Cement sector information to remove from supplysectors input table") %>%
      add_units("NA") %>%
      add_comments("Cement supply sector information for region CHINA") %>%
      add_legacy_name("L2321.DeleteSupplysector_CHINAcement") %>%
      add_precursors("L2321.Supplysector_cement") ->
      L2321.DeleteSupplysector_CHINAcement

    L2321.DeleteFinalDemand_CHINAcement %>%
      add_title("Cement sector information to remove from energy.final.demand input table") %>%
      add_units("NA") %>%
      add_comments("Cement sector information from the energy.final.demand for region CHINA ") %>%
      add_legacy_name("L2321.DeleteFinalDemand_CHINAcement") %>%
      add_precursors("L2321.PerCapitaBased_cement") ->
      L2321.DeleteFinalDemand_CHINAcement

    L2321.StubTechProd_cement_CHINA %>%
      add_title("Calibrated cement stub technologies for cement producing provinces") %>%
      add_units("NA") %>%
      add_comments("Added cement calibrated technology coefficients to cement production by province") %>%
      add_legacy_name("L2321.StubTechProd_cement_CHINA") %>%
      add_precursors("energy/calibrated_techs", "L1321.out_Mt_province_cement_Yh") ->
      L2321.StubTechProd_cement_CHINA

    L2321.StubTechCoef_cement_CHINA %>%
      add_title("Cement production technologies coefficients by province") %>%
      add_units("coefficient = GJ/kg (gigajoules per kilogram of cement)") %>%
      add_comments("Linearly interpolated input-output coefficients for future years") %>%
      add_comments("Matched input-output coefficients with stub technologies of cement by region / supplysector / minicam.energy.input") %>%
      add_comments("Rename markets with regional gird name if using regional regional fuel markets") %>%
      add_legacy_name("L2321.StubTechCoef_cement_CHINA") %>%
      add_precursors("L2321.StubTech_cement", "L1321.IO_GJkg_province_cement_F_Yh",
                     "gcam-china/province_names_mappings", "energy/A321.globaltech_coef") ->
      L2321.StubTechCoef_cement_CHINA

    L2321.StubTechCalInput_cement_heat_CHINA %>%
      add_title("Calibrated input of fuel consumption for producing heat in cement production") %>%
      add_units("calibrated.value = exajoules") %>%
      add_comments("Subset energy inputs to cement production to include heat producing technologies") %>%
      add_comments("Added sector information from calibrated technology mapping file") %>%
      add_comments("Added share weight information") %>%
      add_legacy_name("L2321.StubTechCalInput_cement_heat_CHINA") %>%
      add_precursors("L2321.StubTech_cement", "L1321.IO_GJkg_province_cement_F_Yh",
                     "gcam-china/province_names_mappings", "L1321.in_EJ_province_cement_F_Y", "energy/calibrated_techs") ->
      L2321.StubTechCalInput_cement_heat_CHINA

    L2321.StubTechMarket_cement_CHINA %>%
      add_title("Market names for the consumed for heat by the province cement sectors") %>%
      add_units("NA") %>%
      add_comments("Select heat producing technologies used in province cement production") %>%
      add_comments("Add sector information from calibrated technology mapping file") %>%
      add_comments("Add market name based on region or regional grid if using regional regional fuel markets") %>%
      add_legacy_name("L2321.StubTechMarket_cement_CHINA") %>%
      add_precursors("L2321.StubTech_cement", "L1321.IO_GJkg_province_cement_F_Yh",
                     "gcam-china/province_names_mappings", "energy/calibrated_techs", "L2321.StubTech_cement_CHINA") ->
      L2321.StubTechMarket_cement_CHINA

    L2321.BaseService_cement_CHINA %>%
      add_title("Base-year service output of cement final demand") %>%
      add_units("NA") %>%
      add_comments("Base service is equal to the output of the cement supplysector added final demand to calibrated cement stub technologies for cement producing provinces input table") %>%
      add_legacy_name("L2321.BaseService_cement_CHINA") %>%
      add_precursors("energy/calibrated_techs", "L1321.out_Mt_province_cement_Yh", "energy/A321.demand") ->
      L2321.BaseService_cement_CHINA

    L2321.Supplysector_cement_CHINA %>%
      add_title("Supply sector information for cement sector in cement producing provinces") %>%
      add_units("NA") %>%
      add_comments("Expanded supply sector information to cement producing provinces in region CHINA") %>%
      add_legacy_name("L2321.Supplysector_cement_CHINA") %>%
      add_precursors("L2321.Supplysector_cement", "L1321.out_Mt_province_cement_Yh") ->
      L2321.Supplysector_cement_CHINA

    L2321.FinalEnergyKeyword_cement_CHINA %>%
      add_title("Subsector logit exponents of cement sector in cement producing provinces") %>%
      add_units("Unitless") %>%
      add_comments("Expanded supply sector keywords information cement producing provinces in region CHINA") %>%
      add_legacy_name("L2321.FinalEnergyKeyword_cement_CHINA") %>%
      add_precursors("L2321.FinalEnergyKeyword_cement", "L1321.out_Mt_province_cement_Yh") ->
      L2321.FinalEnergyKeyword_cement_CHINA

    L2321.SubsectorLogit_cement_CHINA %>%
      add_title("Supply sector keywords for cement sector in cement producing provinces") %>%
      add_units("Unitless") %>%
      add_comments("Expanded subsector logit exponents to to cement producing provinces in region CHINA") %>%
      add_legacy_name("L2321.SubsectorLogit_cement_CHINA") %>%
      add_precursors("L2321.SubsectorLogit_cement", "L1321.out_Mt_province_cement_Yh") ->
      L2321.SubsectorLogit_cement_CHINA

    L2321.SubsectorShrwtFllt_cement_CHINA %>%
      add_title("Subsector shareweights of cement sector in cement producing provinces") %>%
      add_units("Unitless") %>%
      add_comments("Expanded subsector shareweights to to cement producing provinces in region CHINA") %>%
      add_legacy_name("L2321.SubsectorShrwtFllt_cement_CHINA") %>%
      add_precursors("L2321.SubsectorShrwtFllt_cement", "L1321.out_Mt_province_cement_Yh") ->
      L2321.SubsectorShrwtFllt_cement_CHINA

    L2321.SubsectorInterp_cement_CHINA %>%
      add_title("Subsector shareweight interpolation of cement sector in cement producing provinces") %>%
      add_units("NA") %>%
      add_comments("Expanded subsector shareweight interpolation to cement producing provinces in region CHINA") %>%
      add_legacy_name("L2321.SubsectorInterp_cement_CHINA") %>%
      add_precursors("L2321.SubsectorInterp_cement", "L1321.out_Mt_province_cement_Yh") ->
      L2321.SubsectorInterp_cement_CHINA

    L2321.StubTech_cement_CHINA %>%
      add_title("Identification of stub technologies of cement in cement producing provinces") %>%
      add_units("NA") %>%
      add_comments("Expanded identification of stub technologies of cement to cement producing provinces in region CHINA") %>%
      add_legacy_name("L2321.StubTech_cement_CHINA") %>%
      add_precursors("L2321.StubTech_cement", "L1321.out_Mt_province_cement_Yh") ->
      L2321.StubTech_cement_CHINA

    L2321.PerCapitaBased_cement_CHINA %>%
      add_title("Per-capita based flag for cement exports final demand in cement producing provinces") %>%
      add_units("NA") %>%
      add_comments("Expanded Per-capita based flag for cement exports final demand to cement producing provinces in region CHINA") %>%
      add_legacy_name("L2321.PerCapitaBased_cement_CHINA") %>%
      add_precursors("L2321.PerCapitaBased_cement", "L1321.out_Mt_province_cement_Yh") ->
      L2321.PerCapitaBased_cement_CHINA

    L2321.PriceElasticity_cement_CHINA %>%
      add_title("Price elasticity for cement in cement producing provinces") %>%
      add_units("Unitless") %>%
      add_comments("Expanded price elasticity for cement to cement producing provinces in region CHINA") %>%
      add_legacy_name("L2321.PriceElasticity_cement_CHINA") %>%
      add_precursors("L2321.PriceElasticity_cement", "L1321.out_Mt_province_cement_Yh") ->
      L2321.PriceElasticity_cement_CHINA

    L2321.IncomeElasticity_cement_gcam3_CHINA %>%
      add_title("Price elasticity for cement in cement producing provinces") %>%
      add_units("Unitless") %>%
      add_comments("Expanded price elasticity for cement to cement producing provinces in region CHINA") %>%
      add_legacy_name("L2321.IncomeElasticity_cement_gcam3_CHINA") %>%
      add_precursors("L2321.IncomeElasticity_cement_gcam3", "L1321.out_Mt_province_cement_Yh") ->
      L2321.IncomeElasticity_cement_gcam3_CHINA

    return_data(L2321.DeleteSupplysector_CHINAcement, L2321.DeleteFinalDemand_CHINAcement,
                L2321.StubTechProd_cement_CHINA, L2321.StubTechCoef_cement_CHINA,
                L2321.StubTechCalInput_cement_heat_CHINA, L2321.StubTechMarket_cement_CHINA,
                L2321.BaseService_cement_CHINA, L2321.Supplysector_cement_CHINA,
                L2321.FinalEnergyKeyword_cement_CHINA, L2321.SubsectorLogit_cement_CHINA,
                L2321.SubsectorShrwtFllt_cement_CHINA, L2321.SubsectorInterp_cement_CHINA,
                L2321.StubTech_cement_CHINA, L2321.PerCapitaBased_cement_CHINA,
                L2321.PriceElasticity_cement_CHINA, L2321.IncomeElasticity_cement_gcam3_CHINA)
  } else {
    stop("Unknown command")
  }
}
