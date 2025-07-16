# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_batch_water_demand_municipal_xml
#'
#' Construct XML data structure for \code{water_demand_municipal.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_demand_municipal_CHINA.xml}.
#' @author Yuqin Li June 2025
module_gcamchina_batch_water_demand_municipal_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/L245.Supplysector_CHINA",
             FILE = "gcam-china/L245.SubsectorLogit_CHINA",
             FILE = "gcam-china/L245.SubsectorShrwtFllt_CHINA",
             FILE = "gcam-china/L245.TechShrwt_CHINA",
             FILE = "gcam-china/L245.TechCoef_CHINA",
             FILE = "gcam-china/L245.TechCost_CHINA",
             FILE = "gcam-china/L245.DeleteSupplysector_CHINA",
             FILE = "gcam-china/L245.DeleteFinalDemand_CHINA",
             FILE = "gcam-china/L245.PerCapitaBased_CHINA",
             FILE = "gcam-china/L245.BaseService_CHINA",
             FILE = "gcam-china/L245.IncomeElasticity_CHINA",
             FILE = "gcam-china/L245.PriceElasticity_CHINA",
             FILE = "gcam-china/L245.aeei_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_demand_municipal_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L245.Supplysector_CHINA <- get_data(all_data, "gcam-china/L245.Supplysector_CHINA")
    L245.SubsectorLogit_CHINA <- get_data(all_data, "gcam-china/L245.SubsectorLogit_CHINA")
    L245.SubsectorShrwtFllt_CHINA <- get_data(all_data, "gcam-china/L245.SubsectorShrwtFllt_CHINA")
    L245.TechShrwt_CHINA <- get_data(all_data, "gcam-china/L245.TechShrwt_CHINA")
    L245.TechCoef_CHINA <- get_data(all_data, "gcam-china/L245.TechCoef_CHINA")
    L245.TechCost_CHINA <- get_data(all_data, "gcam-china/L245.TechCost_CHINA")
    L245.DeleteSupplysector_CHINA <- get_data(all_data, "gcam-china/L245.DeleteSupplysector_CHINA")
    L245.DeleteFinalDemand_CHINA <- get_data(all_data, "gcam-china/L245.DeleteFinalDemand_CHINA")
    L245.PerCapitaBased_CHINA <- get_data(all_data, "gcam-china/L245.PerCapitaBased_CHINA")
    L245.BaseService_CHINA <- get_data(all_data, "gcam-china/L245.BaseService_CHINA")
    L245.IncomeElasticity_CHINA <- get_data(all_data, "gcam-china/L245.IncomeElasticity_CHINA")
    L245.PriceElasticity_CHINA <- get_data(all_data, "gcam-china/L245.PriceElasticity_CHINA")
    L245.aeei_CHINA <- get_data(all_data, "gcam-china/L245.aeei_CHINA")

    # ===================================================

    # Produce outputs
    create_xml("water_demand_municipal_CHINA.xml") %>%
      add_logit_tables_xml(L245.Supplysector_CHINA, "Supplysector") %>%
      add_logit_tables_xml(L245.SubsectorLogit_CHINA, "SubsectorLogit") %>%
      add_xml_data(L245.SubsectorShrwtFllt_CHINA, "SubsectorShrwtFllt") %>%
      add_xml_data(L245.TechShrwt_CHINA, "TechShrwt") %>%
      add_xml_data(L245.TechCoef_CHINA, "TechCoef") %>%
      add_xml_data(L245.TechCost_CHINA, "TechCost") %>%
      add_xml_data(L245.DeleteSupplysector_CHINA, "DeleteSupplysector") %>%
      add_xml_data(L245.DeleteFinalDemand_CHINA, "DeleteFinalDemand") %>%
      add_xml_data(L245.PerCapitaBased_CHINA, "PerCapitaBased") %>%
      add_xml_data(L245.BaseService_CHINA, "BaseService") %>%
      add_xml_data(L245.IncomeElasticity_CHINA, "IncomeElasticity") %>%
      add_xml_data(L245.PriceElasticity_CHINA, "PriceElasticity") %>%
      add_xml_data(L245.aeei_CHINA, "aeei") %>%
      add_precursors("gcam-china/L245.Supplysector_CHINA", "gcam-china/L245.SubsectorLogit_CHINA", "gcam-china/L245.SubsectorShrwtFllt_CHINA",
                     "gcam-china/L245.TechShrwt_CHINA", "gcam-china/L245.TechCoef_CHINA", "gcam-china/L245.TechCost_CHINA", "gcam-china/L245.DeleteSupplysector_CHINA","gcam-china/L245.DeleteFinalDemand_CHINA", "gcam-china/L245.PerCapitaBased_CHINA",
                     "gcam-china/L245.BaseService_CHINA", "gcam-china/L245.IncomeElasticity_CHINA", "gcam-china/L245.PriceElasticity_CHINA", "gcam-china/L245.aeei_CHINA") ->
      water_demand_municipal_CHINA.xml

    return_data(water_demand_municipal_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
