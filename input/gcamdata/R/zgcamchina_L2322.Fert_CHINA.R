# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L2322.Fert
#'
#' Produce tables to create the N fertilizer sector in GCAM-CHINA.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2322.DeleteSubsector_CHINAFert}, \code{L2322.FinalEnergyKeyword_CHINAFert}, \code{L2322.FinalEnergyKeyword_Fert_CHINA}, \code{L2322.StubTech_Fert_CHINA}, \code{L2322.SubsectorLogit_CHINAFert}, \code{L2322.SubsectorShrwtFllt_CHINAFert}, \code{L2322.TechShrwt_CHINAFert}, \code{L2322.Production_CHINAFert}, \code{L2322.TechCoef_CHINAFer}, \code{L2322.StubTechProd_Fert_CHINA}, \code{L2322.StubTechCoef_Fert_CHINA}, \code{L2322.StubTechMarket_Fert_CHINA}, \code{L2322.SubsectorLogit_Fert_CHINA}, \code{L2322.Supplysector_Fert_CHINA}, \code{L2322.SubsectorShrwtFllt_Fert_CHINA}, \code{L2322.SubsectorInterp_Fert_CHINA},\code{L2322.SubsectorInterp_CHINAFert}
#'  The corresponding file in the original data system was \code{L2322.Fert_CHINA.R} (gcam-china level2).
#' @details This chunk produces tables to create the N fertilizer sector in GCAM-CHINA.
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter if_else left_join mutate select
#' @importFrom tidyr gather spread
#' @author YangLiu December 2019
module_gcamchina_L2322.Fert <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "energy/calibrated_techs",
             FILE = "energy/A322.globaltech_coef",
             "L2322.Supplysector_Fert",
             "L2322.FinalEnergyKeyword_Fert",
             "L2322.SubsectorLogit_Fert",
             "L2322.SubsectorShrwtFllt_Fert",
             "L2322.SubsectorInterp_Fert",
             "L2322.StubTech_Fert",
             "L1322.IO_GJkg_province_Fert_F_Yh",
             "L1322.out_Mt_province_Fert_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2322.DeleteSubsector_CHINAFert",
             "L2322.FinalEnergyKeyword_CHINAFert",
             "L2322.FinalEnergyKeyword_Fert_CHINA",
             "L2322.StubTech_Fert_CHINA",
             "L2322.SubsectorLogit_CHINAFert",
             "L2322.SubsectorShrwtFllt_CHINAFert",
             "L2322.TechShrwt_CHINAFert",
             "L2322.Production_CHINAFert",
             "L2322.TechCoef_CHINAFert",
             "L2322.StubTechProd_Fert_CHINA",
             "L2322.StubTechCoef_Fert_CHINA",
             "L2322.StubTechMarket_Fert_CHINA",
             "L2322.SubsectorLogit_Fert_CHINA",
             "L2322.Supplysector_Fert_CHINA",
             "L2322.SubsectorShrwtFllt_Fert_CHINA",
             "L2322.SubsectorInterp_Fert_CHINA",
             "L2322.SubsectorInterp_CHINAFert"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = T)
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs", strip_attributes = T)
    A322.globaltech_coef <- get_data(all_data, "energy/A322.globaltech_coef", strip_attributes = T)
    L2322.Supplysector_Fert <- get_data(all_data, "L2322.Supplysector_Fert", strip_attributes = T)
    L2322.FinalEnergyKeyword_Fert <- get_data(all_data, "L2322.FinalEnergyKeyword_Fert", strip_attributes = T)
    L2322.SubsectorLogit_Fert <- get_data(all_data, "L2322.SubsectorLogit_Fert", strip_attributes = T)
    L2322.SubsectorShrwtFllt_Fert <- get_data(all_data, "L2322.SubsectorShrwtFllt_Fert", strip_attributes = T)
    L2322.SubsectorInterp_Fert <- get_data(all_data, "L2322.SubsectorInterp_Fert", strip_attributes = T)
    L2322.StubTech_Fert <- get_data(all_data, "L2322.StubTech_Fert", strip_attributes = T)
    L1322.IO_GJkg_province_Fert_F_Yh <- get_data(all_data, "L1322.IO_GJkg_province_Fert_F_Yh", strip_attributes = T)
    L1322.out_Mt_province_Fert_Yh <- get_data(all_data, "L1322.out_Mt_province_Fert_Yh", strip_attributes = T)

    # Silence package checks
    regions <- supplysector <- subsector <- province <- value <- year <- subs.share.weights <-
      technology <- share.weight.year <- minicam.energy.input <- coefficient <- market.name <-
      sector <- fuel <- stub.technology <- grid.region <- region <- calOutputValue <-
      subs.share.weight <- tech.share.weight <- logit.year.fillout <- logit.exponent <- logit.type <- NULL


    # ===================================================
    # In the old data system this chunk processed the user defined outputs,
    # L2322.SubsectorInterpTo_Fert and L2322.SubsectorShrwt_Fert, that were produced by
    # the upstream chunk, L2322.Fert. However as per discission with P. Kyle
    # and K. Calvin these outputs were removed from the upstream chunk and therefore are no
    # longer processed by this chunk.

    # In the GCAM region CHINA N fertilizer is retained as a sector, as is the Imports subsector
    # but the the fuel subsectors will be deleted and replaced with province subsectors. Subset the
    # subsector logit exponents of fertilizer sector for the fuel subsectors to be removed in
    # GCAM-CHINA.
    L2322.SubsectorLogit_Fert %>%
      filter(region == gcamchina.REGION, supplysector == gcamchina.FERT_NAME, subsector != "Imports") %>%
      mutate(region = region) %>%
      select(region, supplysector, subsector) ->
      L2322.DeleteSubsector_CHINAFert

    # Subset the supply sector keywords for fertilizer sector in the CHINA region.
    L2322.FinalEnergyKeyword_Fert %>%
      filter(region == gcamchina.REGION) %>%
      mutate(final.energy = "none") ->
      L2322.FinalEnergyKeyword_CHINAFert


    # indicates fertilizer production, create a tibble of the fertilizer producing provinces. This
    # tibble will be used to create the N fertilizer tables for GCAM-CHINA.
    L1322.out_Mt_province_Fert_Yh %>%
      select(province) %>%
      distinct ->
      Fert_provinces

    # Select the supply sector information for fertilizer sector within China and expand to all of the
    # provinces that are fertilizer producers, then create subsector from province and fertilizer name.
    L2322.Supplysector_Fert %>%
      filter(region == gcamchina.REGION, supplysector == gcamchina.FERT_NAME) %>%
      select(region, supplysector) %>%
      repeat_add_columns(Fert_provinces) %>%
      mutate(subsector = paste(province, gcamchina.FERT_NAME)) ->
      L2322.Supplysector_Fert_provinces

    # Now add the logit table information to the province fertilizer supply sector data frame.
    L2322.Supplysector_Fert_provinces %>%
      mutate(logit.year.fillout = min(MODEL_YEARS),
             logit.exponent = gcamchina.FERT_LOGIT_EXP,
             logit.type = NA) %>%
      select(region, supplysector, subsector, logit.year.fillout, logit.exponent, logit.type) ->
      L2322.SubsectorLogit_CHINAFert

    # Create the subsector default share-weights for the using the min base years
    # for the fill out year.
    L2322.SubsectorLogit_CHINAFert %>%
      select(region, supplysector, subsector) %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS),
             share.weight = 1) ->
      L2322.SubsectorShrwtFllt_CHINAFert

    # Use the subsector logit exponents of fertilizer sector to create
    # a table of the subsector default technology share-weights for the China.
    # that will be interpolated.
    L2322.SubsectorLogit_CHINAFert %>%
      select(region, supplysector, subsector) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") ->
      L2322.SubsectorInterp_CHINAFert


    # Expand the supply sector and subsector share weights to all model years.
    L2322.SubsectorLogit_CHINAFert %>%
      select(region, supplysector, subsector) %>%
      mutate(technology = subsector) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1) ->
      L2322.TechShrwt_CHINAFert


    # Calibrated production in CHINA region fertilizer sector (consuming output of provinces)
    L1322.out_Mt_province_Fert_Yh %>%
      filter(year %in% MODEL_BASE_YEARS ) %>%
      group_by(province,sector,year) %>%
      summarise(value = sum(value)) %>%
      complete(nesting(province), year = c(MODEL_BASE_YEARS)) %>%
      arrange(province, year) %>%
      group_by(province) %>%
      mutate(value = approx_fun(year, value)) %>%
      ungroup() %>%
      mutate(calOutputValue = signif(value, aglu.DIGITS_LAND_USE)) %>%
      select(-value) %>%
      mutate(region = gcamchina.REGION, supplysector = gcamchina.FERT_NAME) %>%
      unite(subsector, province, supplysector, sep = " ", remove = FALSE) ->
      L2322.Production_CHINAFert


    # Add technology and subsector share weights and other logit table
    # information to the calibrated output production for fertilizer in the
    # CHINA region fertilizer sector.
    L2322.Production_CHINAFert %>%
      mutate(technology = subsector,
             input = gcamchina.FERT_NAME,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue == 0, 0, 1),
             tech.share.weight = subs.share.weight) %>%
      select(region, supplysector, subsector, technology, year, calOutputValue,
             share.weight.year, subs.share.weight,
             tech.share.weight) ->
      L2322.Production_CHINAFert


    # Add minicam energy input information and coefficient to the
    # to the technology share weight data frame.
    L2322.TechShrwt_CHINAFert %>%
      mutate(minicam.energy.input = gcamchina.FERT_NAME,
             coefficient = 1) %>%
      # Parse out province market name from the fertilizer subsector.
      mutate(market.name = substr(start = 1, stop = 2, subsector)) %>%
      select(region, supplysector, subsector, technology, year,
             minicam.energy.input, coefficient, market.name) ->
      L2322.TechCoef_CHINAFert


    # The Fert_CHINA_processing function replaces the "identical processing for loop"
    # in the old data system. The function inputs include an input data frame to be
    # checked and processed if deemed necessary and a list of the fertilizer producing
    # provinces.

    Fert_CHINA_processing <- function(data, Fert_provinces) {

      # Subset the input data frame for the CHINA region and N fertilizer supply sector.
      # If the subsetted data frame does not contain any fertilizer supplysector information
      # for the CHINA region then it is assumed that the data frame has already been
      # processed, and the input data frame is returned as is.
      check_df <- dplyr::filter(data, region == gcamchina.REGION & supplysector == gcamchina.FERT_NAME)

      if(nrow(check_df) == 0) {
        # This does not change the entries of the data frame but will strip the attributes
        # from the input data frame.
        new_data <- mutate(data, region = region)
      } else {

        # If the data frame contains CHINA region information for the N fertilizer
        # supply sector then expand the input data to all provinces then subset for
        # the fertilizer producing provinces only.

        # Save the column names for the input data frame.
        df_names <- names(data)

        # Subset for observations in the CHINA region and expand all of the
        # input data frame columns to all CHINA provinces and then subset by the
        # fertilizer producing provinces.
        # province-level Exports_fertilizer sector should be excluded
        data %>%
          filter(region == gcamchina.REGION, supplysector == gcamchina.FERT_NAME) %>%
          write_to_all_provinces(names = df_names, gcamchina.PROVINCES_ALL) %>%
          filter(region %in% Fert_provinces[["province"]]) ->
          new_df
        # province-level N fertilizer should not include the Imports subsector.
        if( "subsector" %in% names( new_df ) )   new_df %>% filter(subsector != "Imports" ) -> new_df

      }
      return(new_df)
    } # end of function


    # Use the Fert_CHINA_processing function to check and or process the following data frames so that
    # all of the output data frames contain information for all fertilizer producing provinces without
    # the Imports subsector if applicable.
    L2322.FinalEnergyKeyword_Fert_CHINA <- Fert_CHINA_processing(L2322.FinalEnergyKeyword_Fert, Fert_provinces)
    L2322.Supplysector_Fert_CHINA <- Fert_CHINA_processing(L2322.Supplysector_Fert, Fert_provinces)
    L2322.SubsectorLogit_Fert_CHINA <- Fert_CHINA_processing(L2322.SubsectorLogit_Fert, Fert_provinces)
    L2322.StubTech_Fert_CHINA <- Fert_CHINA_processing(L2322.StubTech_Fert, Fert_provinces)
    L2322.SubsectorShrwtFllt_Fert_CHINA <- Fert_CHINA_processing(L2322.SubsectorShrwtFllt_Fert, Fert_provinces)
    L2322.SubsectorInterp_Fert_CHINA <- Fert_CHINA_processing(L2322.SubsectorInterp_Fert, Fert_provinces)


    # Create the logit table for the calibrated province fertilizer production.
    #
    # Start by formating the province fertilizer production by subsetting for model base years,
    # rounding to the appropriate digits, and adding region information.
    L1322.out_Mt_province_Fert_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calOutputValue = signif(value, digits = gcamchina.DIGITS_CALOUTPUT),
             region = province) ->
      L2322.StubTechProd_Fert_CHINA

    # Next combine the formated province fertilizer production data frame with the mapping form
    # calibrated intermediate sectors and fuels to supplysector / subsector / technology / input in GCAM
    # data frame.
    L2322.StubTechProd_Fert_CHINA %>%
      left_join_error_no_match(calibrated_techs %>% select(sector, fuel, supplysector, technology, subsector),
                               by = c("fuel", "sector")) ->
      L2322.StubTechProd_Fert_CHINA

    # Lastly, add the logit table information.
    L2322.StubTechProd_Fert_CHINA %>%
      mutate(stub.technology = technology,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(region, supplysector, subsector, stub.technology, year, calOutputValue, share.weight.year,
             subs.share.weight, tech.share.weight) ->
      L2322.StubTechProd_Fert_CHINA


    # Create the logit table for the coefficients of fertilizer production technologies
    #
    # Start by subsetting the province fertilizer input-output coefficient data frame for model base years
    # and rounding the input-output coefficient value to the appropriate digits.
    L1322.IO_GJkg_province_Fert_F_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(coefficient = signif(value, aglu.DIGITS_LAND_USE)) %>%
      select(-value) %>%
      mutate(region = province) ->
      L2322.StubTechCoef_Fert_CHINA

    # Next combine the formated input-output data frame with the mapping form calibrated
    # intermediate sectors and fuels to supplysector / subsector / technology / input in GCAM
    # data frame.
    L2322.StubTechCoef_Fert_CHINA %>%
      left_join(calibrated_techs %>% select(supplysector, subsector, technology, minicam.energy.input, sector, fuel),
                by = c("fuel", "sector")) ->
      L2322.StubTechCoef_Fert_CHINA

    # Next add stub.technology and market.name columns and select the columns to include in the final
    # output.
    L2322.StubTechCoef_Fert_CHINA %>%
      mutate(stub.technology = technology, market.name = gcamchina.REGION) %>%
      select(region, supplysector, subsector, stub.technology,
             year, minicam.energy.input, coefficient, market.name) %>%
      # replace market name with the grid region name if the minicam.energy.input is
      # considered a regional fuel market
      left_join_error_no_match(province_names_mappings %>%
                                 select(province, grid.region),
                               by = c("region" = "province")) %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamchina.REGIONAL_FUEL_MARKETS,
             grid.region, market.name)) %>%
      select(-grid.region) ->
	  L2322.StubTechCoef_Fert_CHINA


    # Create a table of the market for the fuel inputs into the province fertilizer sectors
    #
    # Start by expanding the region / supplysector / stub.technology for the fertilizer producing provinces
    # to all model years.
    L2322.StubTech_Fert_CHINA %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      L2322.StubTechMarket_Fert_CHINA

    # Then add minicam.energy.input coefficients from the fertilizer production default coefficients data frame
    # by supplysector, subsector, and technology, then add CHINA as the default market name.

    L2322.StubTechMarket_Fert_CHINA %>%
      # use left_join because hydro has two minicam.energy.input (H2 industrial and wholesale gas)
      # so the number of rows will change
      left_join(A322.globaltech_coef %>%
                  select(supplysector, subsector, technology, minicam.energy.input),
                by = c("supplysector", "subsector", c("stub.technology" = "technology"))) %>%
      mutate(market.name = region) ->
	  L2322.StubTechMarket_Fert_CHINA

    # Fuels are from the CHINA markets, except for regional fuel markets
    L2322.StubTechMarket_Fert_CHINA %>%
      mutate(market.name = gcamchina.REGION) %>%
      select(region, supplysector, subsector, stub.technology, year, minicam.energy.input, market.name) %>%
      # replace market name with the  region name if the minicam.energy.input is
      # considered a regional fuel market
      mutate(market.name = if_else(minicam.energy.input %in% gcamchina.REGIONAL_FUEL_MARKETS,
                                   region, market.name),
             market.name = if_else(minicam.energy.input %in% gcamchina.PROVINCE_FUEL_MARKETS,
                                   region, market.name)) ->
      L2322.StubTechMarket_Fert_CHINA

    # ===================================================

    # Produce outputs
    L2322.DeleteSubsector_CHINAFert %>%
      add_title("Subsector logit exponents of fertilizer sector to remove from GCAM-CHINA") %>%
      add_units("NA") %>%
      add_comments("Subset L2322.SubsectorLogit_Fert for all observation other than subsector Imports and supplysector N fertilizer in the China") %>%
      add_legacy_name("L2322.DeleteSubsector_CHINAFert") %>%
      add_precursors("L2322.SubsectorLogit_Fert") ->
      L2322.DeleteSubsector_CHINAFert

    L2322.FinalEnergyKeyword_CHINAFert %>%
      add_title("Supply sector keywords for fertilizer sector for GCAM CHINA") %>%
      add_units("NA") %>%
      add_comments("Supply sector keywords for fertilizer are subset for the CHINA region.") %>%
      add_legacy_name("L2322.FinalEnergyKeyword_CHINAFert") %>%
      add_precursors("L2322.FinalEnergyKeyword_Fert") ->
      L2322.FinalEnergyKeyword_CHINAFert

    L2322.FinalEnergyKeyword_Fert_CHINA %>%
      add_title("Supply sector keywords for fertilizer sector for fertilizer producing provinces in region CHINA") %>%
      add_units("NA") %>%
      add_comments("Supply sector keywords for fertilizer are subset for the CHINA region then expanded for all fertilizer producing provinces") %>%
      add_legacy_name("L2322.FinalEnergyKeyword_Fert_CHINA") %>%
      add_precursors("L2322.FinalEnergyKeyword_Fert", "L1322.out_Mt_province_Fert_Yh") ->
      L2322.FinalEnergyKeyword_Fert_CHINA

    L2322.StubTech_Fert_CHINA %>%
      add_title("Stub-technology (gas and gas CCS) for fertilizer sector") %>%
      add_units("NA") %>%
      add_comments("Stub-technology (coal, coal CCS, and etc.) for fertilizer sector in region CHINA expanded to all fertilizer producing provinces.") %>%
      add_comments("Delete the fuel subsector and replace with province  fuel subsectors will be deleted and replaced with the relevant province subsectors (gas).") %>%
      add_legacy_name("L2322.StubTech_Fert_CHINA") %>%
      add_precursors("L2322.StubTech_Fert", "L1322.out_Mt_province_Fert_Yh") ->
      L2322.StubTech_Fert_CHINA

    L2322.SubsectorLogit_CHINAFert %>%
      add_title("Subsector logit exponents of fertilizer sector for GCAM CHINA") %>%
      add_units("NA") %>%
      add_comments("For fertilizer sector in region CHINA, the subsector logit exponents are expanded for China provinces with fertilizer census data.") %>%
      add_legacy_name("L2322.SubsectorLogit_CHINAFert") %>%
      add_precursors("L2322.Supplysector_Fert", "L1322.out_Mt_province_Fert_Yh") ->
      L2322.SubsectorLogit_CHINAFert

    L2322.SubsectorShrwtFllt_CHINAFert %>%
      add_title("Subsector share-weight fill out table for fertilizer sector in region CHINA") %>%
      add_units("NA") %>%
      add_comments("Added share-weight fillout and fill out year to fertilizer supplysector / subsector for region CHINA") %>%
      add_legacy_name("L2322.SubsectorShrwtFllt_CHINAFert") %>%
      add_precursors("L2322.Supplysector_Fert", "L1322.out_Mt_province_Fert_Yh") ->
      L2322.SubsectorShrwtFllt_CHINAFert

    L2322.SubsectorInterp_CHINAFert %>%
      add_title("Subsector interpolation table for fertilizer subsector in region CHINA") %>%
      add_units("NA") %>%
      add_comments("Expanded CHINA region subsector to all fertilizer producing provinces in region CHINA") %>%
      add_comments("Added the apply.to, from.year, and interpolation.function columns") %>%
      add_legacy_name("L2322.SubsectorInterp_CHINAFert") %>%
      add_precursors("L2322.Supplysector_Fert", "L1322.out_Mt_province_Fert_Yh") ->
      L2322.SubsectorInterp_CHINAFert

    L2322.TechShrwt_CHINAFert %>%
      add_title("Technology share-weight for N fertilizer in region CHINA") %>%
      add_units("NA") %>%
      add_comments("Added fertilizer producing provinces to subsector and supply sector information in region CHINA") %>%
      add_comments("Expanded for all model years") %>%
      add_legacy_name("L2322.TechShrwt_CHINAFert") %>%
      add_precursors("L2322.Supplysector_Fert", "L1322.out_Mt_province_Fert_Yh") ->
      L2322.TechShrwt_CHINAFert

    L2322.Production_CHINAFert %>%
      add_title("Calibrated fertilizer production in region CHINA by province fertilizer supplysector") %>%
      add_units("calOutputValue = Mt (megatonnes = teragrams)") %>%
      add_comments("Added share-wight information to province fertilizer production") %>%
      add_legacy_name("L2322.Production_CHINAFert") %>%
      add_precursors("L1322.out_Mt_province_Fert_Yh") ->
      L2322.Production_CHINAFert

    L2322.TechCoef_CHINAFert %>%
      add_title("Technology coefficients of CHINA region fertilizer") %>%
      add_units("NA") %>%
      add_comments("Added fertilizer producing provinces to fertilizer subsector & technology in region CHINA") %>%
      add_comments("Added minicam.energy.input, coefficient, and market.name") %>%
      add_legacy_name("L2322.TechCoef_CHINAFert") %>%
      add_precursors("L2322.Supplysector_Fert", "L1322.out_Mt_province_Fert_Yh") ->
      L2322.TechCoef_CHINAFert

    L2322.StubTechProd_Fert_CHINA %>%
      add_title("Calibrated fertilizer production by province") %>%
      add_units("value = Mt (megatonnes = teragrams)") %>%
      add_comments("Added supplysector / technology / subsector information from calibrated_techs mapping file") %>%
      add_comments("Added share weight information to province fertilizer production from L1322.out_Mt_province_Fert_Yh") %>%
      add_legacy_name("L2322.StubTechProd_Fert_CHINA") %>%
      add_precursors("L1322.out_Mt_province_Fert_Yh", "energy/calibrated_techs") ->
      L2322.StubTechProd_Fert_CHINA

    L2322.StubTechCoef_Fert_CHINA %>%
      add_title("Stub-technology input output energy coefficient for fertilizer production in region CHINA") %>%
      add_units("coefficient = GJkg (gigajoules used/kg fertilizer produced)") %>%
      add_comments("Added supplysector / subsector / technology / minicam.energy.input information from calibrated_tech mapping file") %>%
      add_comments("Add market.name, default is CHINA but depending on user defined input, market.name cane be replace with region grid from province_names_mappings ") %>%
      add_legacy_name("L2322.StubTechCoef_Fert_CHINA") %>%
      add_precursors("L1322.out_Mt_province_Fert_Yh", "L1322.IO_GJkg_province_Fert_F_Yh", "gcam-china/province_names_mappings", "energy/calibrated_techs") ->
      L2322.StubTechCoef_Fert_CHINA

    L2322.StubTechMarket_Fert_CHINA %>%
      add_title("Market for the fuel inputs to the province fertilizer sectors by fertilizer producing provinces") %>%
      add_units("NA") %>%
      add_comments("Added minicam.energy.input to subsector/ stub.technology for supplysector expanded to all N fertilizer producing provinces") %>%
      add_comments("Add market.name, default is CHINA but depending on user defined input, market.name cane be replace with region grid from province_names_mappings ") %>%
      add_legacy_name("L2322.StubTechMarket_Fert_CHINA") %>%
      add_precursors("L1322.out_Mt_province_Fert_Yh", "L2322.StubTech_Fert", "gcam-china/province_names_mappings", "energy/A322.globaltech_coef") ->
      L2322.StubTechMarket_Fert_CHINA

    L2322.SubsectorLogit_Fert_CHINA %>%
      add_title("Subsector logit exponents of fertilizer sector for fertilizer producing provinces in region GCAM CHINA") %>%
      add_units("NA") %>%
      add_comments("Replace region from L2322.SubsectorLogit_CHINAFert with the fertilizer producing provinces within region CHINA") %>%
      add_comments("Replace province subsector with gas") %>%
      add_legacy_name("L2322.SubsectorLogit_Fert_CHINA") %>%
      add_precursors("L2322.Supplysector_Fert", "L1322.out_Mt_province_Fert_Yh") ->
      L2322.SubsectorLogit_Fert_CHINA

    L2322.Supplysector_Fert_CHINA %>%
      add_title("Supply sector information for fertilizer sector for fertilizer producing provinces in region CHINA") %>%
      add_units("NA") %>%
      add_comments("Expanded supply sector information for fertilizer sector for region CHINA to all fertilizer producing provinces") %>%
      add_legacy_name("L2322.Supplysector_Fert_CHINA") %>%
      add_precursors("L1322.out_Mt_province_Fert_Yh", "L2322.Supplysector_Fert") ->
      L2322.Supplysector_Fert_CHINA

    L2322.SubsectorShrwtFllt_Fert_CHINA %>%
      add_title("Subsector share-weight fill out table for fertilizer sector in fertilizer producing provinces") %>%
      add_units("NA") %>%
      add_comments("Added share-weight fillout and fill out year to fertilizer supplysector / subsector for region CHINA") %>%
      add_comments("Expanded to all fertilizer producing provinces in region CHINA and subsetted for the relevant province subsector (gas)") %>%
      add_legacy_name("L2322.SubsectorShrwtFllt_Fert_CHINA") %>%
      add_precursors("L2322.SubsectorShrwtFllt_Fert", "L2322.Supplysector_Fert", "L1322.out_Mt_province_Fert_Yh") ->
      L2322.SubsectorShrwtFllt_Fert_CHINA

    L2322.SubsectorInterp_Fert_CHINA %>%
      add_title("Subsector interpolation table for fertilizer producing provinces in region CHINA") %>%
      add_units("NA") %>%
      add_comments("Expanded subsector to fertilizer producing provinces in region CHINA") %>%
      add_comments("Added the apply.to, from.year, and interpolation.function columns") %>%
      add_legacy_name("L2322.SubsectorInterp_Fert_CHINA") %>%
      add_precursors("L2322.SubsectorInterp_Fert", "L2322.Supplysector_Fert", "L1322.out_Mt_province_Fert_Yh") ->
      L2322.SubsectorInterp_Fert_CHINA

    return_data(L2322.DeleteSubsector_CHINAFert, L2322.FinalEnergyKeyword_CHINAFert, L2322.FinalEnergyKeyword_Fert_CHINA,
                L2322.StubTech_Fert_CHINA, L2322.SubsectorLogit_CHINAFert, L2322.SubsectorShrwtFllt_CHINAFert, L2322.TechShrwt_CHINAFert,
                L2322.Production_CHINAFert, L2322.TechCoef_CHINAFert, L2322.StubTechProd_Fert_CHINA,
                L2322.StubTechCoef_Fert_CHINA, L2322.StubTechMarket_Fert_CHINA, L2322.SubsectorLogit_Fert_CHINA,
                L2322.Supplysector_Fert_CHINA, L2322.SubsectorShrwtFllt_Fert_CHINA, L2322.SubsectorInterp_Fert_CHINA,
                L2322.SubsectorInterp_CHINAFert)
    } else {
      stop("Unknown command")
    }
  }
