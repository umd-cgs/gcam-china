#' module_gcam.china_L261.carbon_storage_CHINA
#'
#' China carbon storage
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L261.DeleteSubsector_CHINAC}, \code{L261.Rsrc_province}, \code{L261.RsrcCurves_province},
#' \code{L261.Supplysector_C_CHINA},\code{L261.SubsectorLogit_C_CHINA},\code{L261.SubsectorShrwtFllt_C_CHINA},
#' \code{L261.StubTech_C_CHINA},\code{L261.StubTechMarket_C_CHINA},
#' \code{L261.DeleteRsrc_CHINAC},\code{L261.UnlimitRsrc_CHINA}, \code{L261.GlobalTechCost_C_CHINA},\code{L261.ResTechShrwt_C_CHINA}
#' The corresponding file in the
#' original data system was \code{L261_carbon_storage_CHINA.R} (gcam-china level2).
#' @details China carbon storage model input for GCAM-China energy electricity sectors
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BY Feb 2020


module_gcam.china_L261.carbon_storage_CHINA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE="gcam-china/province_names_mappings",
             FILE="gcam-china/CO2_cost_capacity_province",
             "L161.Cstorage_province",
             "L261.Rsrc",
             "L261.Supplysector_C",
             "L261.SubsectorLogit_C",
             "L261.SubsectorShrwtFllt_C",
             "L261.StubTech_C",
             "L261.GlobalTechCoef_C",
             "L261.GlobalTechCost_C",
             "L261.ResTechShrwt_C"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L261.DeleteSubsector_CHINAC",
             "L261.Rsrc_province",
             "L261.RsrcCurves_province",
             "L261.Supplysector_C_CHINA",
             "L261.SubsectorLogit_C_CHINA",
             "L261.SubsectorShrwtFllt_C_CHINA",
             "L261.StubTech_C_CHINA",
             "L261.StubTechMarket_C_CHINA",
             "L261.DeleteRsrc_CHINAC",
             "L261.UnlimitRsrc_CHINA",
             "L261.GlobalTechCost_C_CHINA",
             "L261.ResTechShrwt_C_CHINA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = T)
    CO2_cost_capacity_province <- get_data(all_data, "gcam-china/CO2_cost_capacity_province", strip_attributes = T)
    L161.Cstorage_province <- get_data(all_data, "L161.Cstorage_province", strip_attributes = T)
    L261.Rsrc <- get_data(all_data, "L261.Rsrc", strip_attributes = T)
    L261.Supplysector_C <- get_data(all_data, "L261.Supplysector_C", strip_attributes = T)
    L261.SubsectorLogit_C <- get_data(all_data, "L261.SubsectorLogit_C", strip_attributes = T)
    L261.SubsectorShrwtFllt_C <- get_data(all_data, "L261.SubsectorShrwtFllt_C", strip_attributes = T)
    L261.StubTech_C <- get_data(all_data, "L261.StubTech_C", strip_attributes = T)
    L261.GlobalTechCoef_C <- get_data(all_data, "L261.GlobalTechCoef_C", strip_attributes = T)
    L261.GlobalTechCost_C <- get_data(all_data, "L261.GlobalTechCost_C", strip_attributes = T)
    L261.ResTechShrwt_C <- get_data(all_data, "L261.ResTechShrwt_C", strip_attributes = T)

    # Silence package notes
    province <- region <- resource <- MtC <- Cost_1990USDtC <- subresource <- grade <-
      available <- extractioncost <- subsector <- stub.technology <- coefficient <- market.name <-
      minicam.energy.input <- NULL

    # ===================================================
    # Perform computations

    # Carbon storage onshore resources are modeled in the provinces with non-zero storage curves
    C_provinces <- L161.Cstorage_province %>%
      select(province) %>%
      unique %>%
      arrange %>%
      unlist

    noC_provinces <- province_names_mappings %>%
      select(province) %>%
      filter(!province %in% C_provinces) %>%
      unique %>%
      arrange %>%
      unlist

    # L261.DeleteRsrc_CHINAC: delete onshore carbon storage in CHINA
    L261.Rsrc %>%
      mutate(region = region) %>% # strip off attributes like title, etc.
      filter(region == gcamchina.REGION) %>%
      select(region, resource) ->
      L261.DeleteRsrc_CHINAC

    # L261.DeleteSubsector_CHINAC: delete onshore carbon storage subsector of carbon storage sector in the CHINA region
    # NOTE: leaving the offshore here so that the CHINA hydrogen sector has a carbon storage market
    L261.SubsectorShrwtFllt_C %>%
      mutate(region = region) %>% # strip off attributes like title, etc.
      filter(region == gcamchina.REGION) %>%
      semi_join(L261.Rsrc, by = c("subsector" = "resource")) %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["Subsector"]]))) ->
      L261.DeleteSubsector_CHINAC

    # Create a vector of province onshore carbon storage subsector with zero storage curve
    # Provinces will be excluded from the onshare carbon storage subsector in the relevant input files below
    prov_Cstorage_nonexist <- paste(noC_provinces, L261.DeleteRsrc_CHINAC$resource[1])

    # L261.Rsrc_province: onshore storage in the provinces
    L261.Rsrc %>%
      filter(region == gcamchina.REGION) %>%
      select(-region) %>%
      # Onshore storage only in the provinces with non-zero storage curves
      repeat_add_columns(tibble(region = C_provinces)) %>%
      mutate(market = region) ->
      L261.Rsrc_province

    # L261.RsrcCurves_province: onshore storage supply curves in the provinces
    L161.Cstorage_province %>%
      mutate(region = province,
             resource = L261.Rsrc_province$resource[1],
             subresource = L261.Rsrc_province$resource[1],
             available = round(MtC, digits = energy.DIGITS_RESOURCE),
             extractioncost = round(Cost_1990USDtC, digits = energy.DIGITS_COST)) %>%
      select(region, resource, subresource, grade, available, extractioncost) ->
      L261.RsrcCurves_province

    # L261.Supplysector_C_CHINA: supplysector information in the provinces
    L261.Supplysector_C %>%
      filter(region == gcamchina.REGION) %>%
      write_to_all_provinces(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), gcamchina.PROVINCES_NOHKMC) ->
      L261.Supplysector_C_CHINA

    # L261.SubsectorLogit_C_CHINA: subsector logit information in the provinces
    L261.SubsectorLogit_C %>%
      filter(region == gcamchina.REGION) %>%
      write_to_all_provinces(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), gcamchina.PROVINCES_NOHKMC) %>%
      #NOTE: This table contains logit values in provinces where no C storage resources may exist at the province level
      left_join_error_no_match(select(province_names_mappings, province), by = c("region" = "province")) %>%
      # Drop the provinces where no carbon storage resources may exist at the grid level
      filter(!paste(region, subsector) %in% prov_Cstorage_nonexist) ->
      L261.SubsectorLogit_C_CHINA

    # L261.SubsectorShrwtFllt_C_CHINA: subsector shareweight information in the provinces
    L261.SubsectorShrwtFllt_C %>%
      filter(region == gcamchina.REGION) %>%
      write_to_all_provinces(c(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]), gcamchina.PROVINCES_NOHKMC) %>%
      # Drop the provinces where no carbon storage resources may exist at the province level
      filter(!paste(region, subsector) %in% prov_Cstorage_nonexist) ->
      L261.SubsectorShrwtFllt_C_CHINA

    # L261.StubTech_C_CHINA: stub technology information for the provinces
    L261.StubTech_C %>%
      filter(region == gcamchina.REGION) %>%
      write_to_all_provinces(c(LEVEL2_DATA_NAMES[["StubTech"]]), gcamchina.PROVINCES_NOHKMC) %>%
      left_join_error_no_match(select(province_names_mappings, province), by = c("region" = "province")) %>%
      # Drop the provinces where no carbon storage resources may exist at the province level
      filter(!paste(region, stub.technology) %in% prov_Cstorage_nonexist) %>%
      select(one_of(c(LEVEL2_DATA_NAMES[["StubTech"]])))->
      L261.StubTech_C_CHINA

    # L261.StubTechMarket_C_CHINA: stub technology market information for the provinces
    L261.StubTech_C_CHINA %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      left_join_error_no_match(select(L261.GlobalTechCoef_C, -coefficient),
                               by = c("supplysector" = "sector.name", "subsector" = "subsector.name", "stub.technology" = "technology", "year")) %>%
      mutate(market.name = region) %>%
      # Replace offshore carbon storage with the CHINA market
      mutate(market.name = replace(market.name, !minicam.energy.input %in% L261.Rsrc_province$resource, gcamchina.REGION)) ->
      L261.StubTechMarket_C_CHINA

    # Toggle the rest of the script tables for adding onshore carbon storage to the CHINA market
    if(gcamchina.USE_UNIFORM_CSTORAGE_MARKET){
      L261.StubTechMarket_C_CHINA <- L261.StubTechMarket_C_CHINA %>%
        mutate(market.name = gcamchina.REGION)

      # L261.UnlimitRsrc: output unit, price unit, and market for unlimited resources
      L261.UnlimitRsrc_CHINA <- tibble(
        region = gcamchina.REGION,
        unlimited.resource = unique(L261.Rsrc$resource),
        output.unit = "MtC",
        price.unit = "1990$/tC",
        market = gcamchina.REGION,
        capacity.factor = 0 )

      # Costs of global technologies
      #"L261.GlobalTechCost_C_CHINA: Costs of onshore carbon storage in CHINA for unlimited onshore
      L261.GlobalTechCost_C_CHINA <- L261.GlobalTechCost_C %>%
        mutate(subsector.name = "onshore carbon-storage", technology = "onshore carbon-storage",
               input.cost = min(CO2_cost_capacity_province$mid.price) / gdp_deflator(2005, base_year = 1990) * emissions.CONV_C_CO2)
    }

    L261.ResTechShrwt_C_CHINA <- L261.ResTechShrwt_C %>%
      filter(region == gcamchina.REGION) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["ResTechShrwt"]], gcamchina.PROVINCES_NOHKMC)

    # ===================================================
    # Produce outputs
    if(gcamchina.USE_UNIFORM_CSTORAGE_MARKET){
      L261.DeleteRsrc_CHINAC %>%
        add_title("Delete onshore carbon storage in the CHINA region") %>%
        add_units("NA") %>%
        add_comments("Carbon storage onshore resources are modeled by provinces") %>%
        add_legacy_name("L261.DeleteDepRsrc_CHINAC") %>%
        add_precursors("L261.Rsrc") ->
        L261.DeleteRsrc_CHINAC

      L261.UnlimitRsrc_CHINA %>%
        add_title("Unlimited onshore resources in the CHINA region") %>%
        add_units("MtC and 1990$/tC") %>%
        add_comments("Only written if using uniform CHINA market for all CStorage") %>%
        add_legacy_name("L261.UnlimitRsrc_CHINA") %>%
        add_precursors("L261.Rsrc") ->
        L261.UnlimitRsrc_CHINA

      L261.GlobalTechCost_C_CHINA %>%
        add_title("Costs of onshore carbon storage in CHINA for unlimited onshore") %>%
        add_units("1990$/tC") %>%
        add_comments("Only written if using uniform CHINA market for all CStorage") %>%
        add_legacy_name("L261.GlobalTechCost_C_CHINA") %>%
        add_precursors("L261.GlobalTechCost_C") ->
        L261.GlobalTechCost_C_CHINA

    }
    else {
      # If gcamchina.USE_UNIFORM_CSTORAGE_MARKET is FALSE,
      # then blank tibbles are produced.
      missing_data() %>%
        add_legacy_name("L261.DeleteDepRsrc_CHINAC") ->
        L261.DeleteRsrc_CHINAC

      missing_data() %>%
        add_legacy_name("L261.UnlimitRsrc_CHINA") ->
        L261.UnlimitRsrc_CHINA

      missing_data() %>%
        add_legacy_name("L261.GlobalTechCost_C_CHINA") %>%
        add_precursors("L261.GlobalTechCost_C",
                       "gcam-china/CO2_cost_capacity_province")->
        L261.GlobalTechCost_C_CHINA
    }

    L261.DeleteSubsector_CHINAC %>%
      add_title("Delete onshore carbon storage subsector of carbon storage sector in the CHINA region") %>%
      add_units("NA") %>%
      add_comments("Keep the offshore here so that the CHINA hydrogen sector has a carbon storage market") %>%
      add_legacy_name("L261.DeleteSubsector_CHINAC") %>%
      add_precursors("L261.SubsectorShrwtFllt_C",
                     "L261.Rsrc") ->
      L261.DeleteSubsector_CHINAC

    L261.Rsrc_province %>%
      add_title("Onshore storage in the provinces") %>%
      add_units("NA") %>%
      add_comments("Onshore storage are modeled only in the provinces with non-zero storage curves") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L261.DepRsrc_provinces") %>%
      add_precursors("L161.Cstorage_province",
                     "L261.Rsrc") ->
      L261.Rsrc_province

    L261.RsrcCurves_province %>%
      add_title("Onshore storage supply curves in the provinces") %>%
      add_units("MtC and 1990USDtC") %>%
      add_comments("Onshore carbon storage availability and extraction costs in each province") %>%
      add_legacy_name("L261.DepRsrcCurves_provinces") %>%
      add_precursors("L161.Cstorage_province") %>%
      same_precursors_as("L261.Rsrc_province") ->
      L261.RsrcCurves_province

    L261.Supplysector_C_CHINA %>%
      add_title("Supplysector information in the China provinces") %>%
      add_units("Unitless") %>%
      add_comments("The same China region values are repeated for each province") %>%
      add_legacy_name("L261.Supplysector_C_CHINA") %>%
      add_precursors("L261.Supplysector_C") ->
      L261.Supplysector_C_CHINA

    L261.SubsectorLogit_C_CHINA %>%
      add_title("Subsector logit information in the provinces") %>%
      add_units("Unitless") %>%
      add_comments("The same CHINA region values are repeated for each provinces") %>%
      add_comments("Provinces where no carbon storage resources may exist are dropped") %>%
      add_legacy_name("L261.SubsectorLogit_C_CHINA") %>%
      add_precursors("L161.Cstorage_province",
                     "gcam-china/province_names_mappings",
                     "L261.SubsectorLogit_C") ->
      L261.SubsectorLogit_C_CHINA

    L261.SubsectorShrwtFllt_C_CHINA %>%
      add_title("Subsector shareweight information in the provinces") %>%
      add_units("Unitless") %>%
      add_comments("The same CHINA region values are repeated for each province") %>%
      add_comments("Provinces where no carbon storage resources may exist are dropped") %>%
      add_legacy_name("L261.SubsectorShrwtFllt_C_CHINA") %>%
      add_precursors("L161.Cstorage_province",
                     "gcam-china/province_names_mappings",
                     "L261.SubsectorShrwtFllt_C") ->
      L261.SubsectorShrwtFllt_C_CHINA

    L261.StubTech_C_CHINA %>%
      add_title("Stub technology information for the provinces") %>%
      add_units("Unitless") %>%
      add_comments("The same CHINA region values are repeated for each province") %>%
      add_comments("Provinces where no carbon storage resources may exist are dropped") %>%
      add_legacy_name("L261.StubTech_C_CHINA") %>%
      add_precursors("L161.Cstorage_province",
                     "gcam-china/province_names_mappings",
                     "L261.StubTech_C") ->
      L261.StubTech_C_CHINA

    L261.StubTechMarket_C_CHINA %>%
      add_title("Stub technology market information for the provinces") %>%
      add_units("Unitless") %>%
      add_comments("Onshore carbon storage is from the provinces") %>%
      add_comments("Offshore carbon storage is from the CHINA market") %>%
      add_legacy_name("L261.StubTechMarket_C_CHINA") %>%
      add_precursors("L261.GlobalTechCoef_C",
                     "L261.Rsrc_province") %>%
      same_precursors_as("L261.StubTech_C_CHINA") ->
      L261.StubTechMarket_C_CHINA

    L261.ResTechShrwt_C_CHINA %>%
      add_title("Technology share-weights for the onshore C storage") %>%
      add_units("NA") %>%
      add_comments("Mostly just to provide a shell of a technology for the resource to use") %>%
      add_precursors("L261.ResTechShrwt_C") ->
      L261.ResTechShrwt_C_CHINA

    return_data(L261.DeleteSubsector_CHINAC, L261.Rsrc_province,
                L261.RsrcCurves_province, L261.Supplysector_C_CHINA, L261.SubsectorLogit_C_CHINA,
                L261.SubsectorShrwtFllt_C_CHINA, L261.StubTech_C_CHINA, L261.StubTechMarket_C_CHINA,
                L261.DeleteRsrc_CHINAC, L261.UnlimitRsrc_CHINA, L261.GlobalTechCost_C_CHINA, L261.ResTechShrwt_C_CHINA)

  } else {
    stop("Unknown command")
  }
}
