# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_batch_cement_CHINA_xml
#'
#' Construct XML data structure for \code{cement_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{cement_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_cement_CHINA_xml.R} (gcamchina XML).
module_gcamchina_batch_cement_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L210.UnlimitRsrc_limestone_CHINA",
             "L210.UnlimitRsrcPrice_limestone_CHINA",
             "L2321.DeleteSupplysector_CHINAcement",
             "L2321.Supplysector_cement_CHINA",
             "L2321.FinalEnergyKeyword_cement_CHINA",
             "L2321.SubsectorLogit_cement_CHINA",
             "L2321.SubsectorShrwtFllt_cement_CHINA",
             "L2321.SubsectorInterp_cement_CHINA",
             "L2321.StubTech_cement_CHINA",
             "L2321.PerCapitaBased_cement_CHINA",
             "L2321.PriceElasticity_cement_CHINA",
             "L2321.IncomeElasticity_cement_gcam3_CHINA",
             "L2321.DeleteFinalDemand_CHINAcement",
             "L2321.StubTechProd_cement_CHINA",
             "L2321.StubTechCoef_cement_CHINA",
             "L2321.StubTechCalInput_cement_heat_CHINA",
             "L2321.StubTechMarket_cement_CHINA",
             "L2321.BaseService_cement_CHINA"))



  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "cement_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L210.UnlimitRsrc_limestone_CHINA <- get_data(all_data, "L210.UnlimitRsrc_limestone_CHINA")
    L210.UnlimitRsrcPrice_limestone_CHINA <- get_data(all_data, "L210.UnlimitRsrcPrice_limestone_CHINA")
    L2321.DeleteSupplysector_CHINAcement <- get_data(all_data, "L2321.DeleteSupplysector_CHINAcement")
    L2321.FinalEnergyKeyword_cement_CHINA <- get_data(all_data, "L2321.FinalEnergyKeyword_cement_CHINA")
    L2321.SubsectorLogit_cement_CHINA <- get_data(all_data, "L2321.SubsectorLogit_cement_CHINA")
    L2321.SubsectorShrwtFllt_cement_CHINA <- get_data(all_data, "L2321.SubsectorShrwtFllt_cement_CHINA")
    L2321.SubsectorInterp_cement_CHINA <- get_data(all_data, "L2321.SubsectorInterp_cement_CHINA")
    L2321.StubTech_cement_CHINA <- get_data(all_data, "L2321.StubTech_cement_CHINA")
    L2321.PerCapitaBased_cement_CHINA <- get_data(all_data, "L2321.PerCapitaBased_cement_CHINA")
    L2321.PriceElasticity_cement_CHINA <- get_data(all_data, "L2321.PriceElasticity_cement_CHINA")
    L2321.IncomeElasticity_cement_gcam3_CHINA <- get_data(all_data, "L2321.IncomeElasticity_cement_gcam3_CHINA")
    L2321.DeleteFinalDemand_CHINAcement <- get_data(all_data, "L2321.DeleteFinalDemand_CHINAcement")
    L2321.Supplysector_cement_CHINA <- get_data(all_data, "L2321.Supplysector_cement_CHINA")
    L2321.StubTechProd_cement_CHINA <- get_data(all_data, "L2321.StubTechProd_cement_CHINA")
    L2321.StubTechCoef_cement_CHINA <- get_data(all_data, "L2321.StubTechCoef_cement_CHINA")
    L2321.StubTechCalInput_cement_heat_CHINA <- get_data(all_data, "L2321.StubTechCalInput_cement_heat_CHINA")
    L2321.StubTechMarket_cement_CHINA <- get_data(all_data, "L2321.StubTechMarket_cement_CHINA")
    L2321.BaseService_cement_CHINA <- get_data(all_data, "L2321.BaseService_cement_CHINA")

    # ===================================================

    # Produce outputs
    create_xml("cement_CHINA.xml") %>%
      add_xml_data(L2321.DeleteSupplysector_CHINAcement, "DeleteSupplysector") %>%
      add_xml_data(L2321.DeleteFinalDemand_CHINAcement, "DeleteFinalDemand") %>%
      add_logit_tables_xml(L2321.Supplysector_cement_CHINA, "Supplysector") %>%
      add_logit_tables_xml(L2321.SubsectorLogit_cement_CHINA, "SubsectorLogit") %>%
      add_xml_data(L2321.FinalEnergyKeyword_cement_CHINA, "FinalEnergyKeyword") %>%
      add_xml_data(L2321.SubsectorShrwtFllt_cement_CHINA, "SubsectorShrwtFllt") %>%
      add_xml_data(L2321.SubsectorInterp_cement_CHINA, "SubsectorInterp") %>%
      add_xml_data(L2321.StubTech_cement_CHINA, "StubTech") %>%
      add_xml_data(L2321.PerCapitaBased_cement_CHINA, "PerCapitaBased") %>%
      add_xml_data(L2321.PriceElasticity_cement_CHINA, "PriceElasticity") %>%
      add_xml_data(L2321.IncomeElasticity_cement_gcam3_CHINA, "IncomeElasticity") %>%
      add_xml_data(L2321.StubTechProd_cement_CHINA, "StubTechProd") %>%
      add_xml_data(L2321.StubTechCoef_cement_CHINA, "StubTechCoef") %>%
      add_xml_data(L2321.StubTechCalInput_cement_heat_CHINA, "StubTechCalInput") %>%
      add_xml_data(L2321.StubTechMarket_cement_CHINA, "StubTechMarket") %>%
      add_xml_data(L2321.BaseService_cement_CHINA, "BaseService") %>%
      add_xml_data(L210.UnlimitRsrc_limestone_CHINA, "UnlimitRsrc") %>%
      add_xml_data(L210.UnlimitRsrcPrice_limestone_CHINA, "UnlimitRsrcPrice") %>%
      add_precursors("L210.UnlimitRsrc_limestone_CHINA",
                     "L210.UnlimitRsrcPrice_limestone_CHINA",
                     "L2321.DeleteSupplysector_CHINAcement",
                     "L2321.Supplysector_cement_CHINA",
                     "L2321.FinalEnergyKeyword_cement_CHINA",
                     "L2321.SubsectorLogit_cement_CHINA",
                     "L2321.SubsectorShrwtFllt_cement_CHINA",
                     "L2321.SubsectorInterp_cement_CHINA",
                     "L2321.StubTech_cement_CHINA",
                     "L2321.PerCapitaBased_cement_CHINA",
                     "L2321.PriceElasticity_cement_CHINA",
                     "L2321.IncomeElasticity_cement_gcam3_CHINA",
                     "L2321.DeleteFinalDemand_CHINAcement",
                     "L2321.StubTechProd_cement_CHINA",
                     "L2321.StubTechCoef_cement_CHINA",
                     "L2321.StubTechCalInput_cement_heat_CHINA",
                     "L2321.StubTechMarket_cement_CHINA",
                     "L2321.BaseService_cement_CHINA") ->
      cement_CHINA.xml

    return_data(cement_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
