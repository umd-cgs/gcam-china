# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_batch_water_td_CHINA_xml
#'
#' Construct XML data structure for \code{water_mapping.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_td_CHINA.xml}.
#' @author Yuqin Li June 2025
module_gcamchina_batch_water_td_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L203.Supplysector_CHINA",
             "L203.SubsectorLogit_CHINA",
             "L203.SubsectorShrwt_CHINA",
             "L203.TechShrwt_CHINA",
             "L203.TechCoef_CHINA",
             "L203.TechPmult_CHINA",
             "L203.DeleteSupplysector_CHINA",
             "L203.DeleteResTechInput_CHINA",
             "L203.DeleteSubsector_CHINA",
             "L203.TechDesalCoef_CHINA",
             "L203.TechDesalShrwt_CHINA",
             "L203.TechDesalCost_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_td_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L203.Supplysector_CHINA <- get_data(all_data, "L203.Supplysector_CHINA")
    L203.SubsectorLogit_CHINA <- get_data(all_data, "L203.SubsectorLogit_CHINA")
    L203.SubsectorShrwt_CHINA <- get_data(all_data, "L203.SubsectorShrwt_CHINA")
    L203.TechShrwt_CHINA <- get_data(all_data, "L203.TechShrwt_CHINA")
    L203.TechCoef_CHINA <- get_data(all_data, "L203.TechCoef_CHINA")
    L203.TechPmult_CHINA <- get_data(all_data,"L203.TechPmult_CHINA")
    L203.DeleteSupplysector_CHINA <- get_data(all_data, "L203.DeleteSupplysector_CHINA")
    L203.DeleteResTechInput_CHINA <- get_data(all_data, "L203.DeleteResTechInput_CHINA")
    L203.DeleteSubsector_CHINA <- get_data(all_data, "L203.DeleteSubsector_CHINA")
    L203.TechDesalCoef_CHINA  <- get_data(all_data, "L203.TechDesalCoef_CHINA")
    L203.TechDesalShrwt_CHINA <- get_data(all_data, "L203.TechDesalShrwt_CHINA")
    L203.TechDesalCost_CHINA <- get_data(all_data, "L203.TechDesalCost_CHINA")

    # ===================================================

    # Produce outputs
    create_xml("water_td_CHINA.xml") %>%
      add_logit_tables_xml(L203.Supplysector_CHINA, "Supplysector") %>%
      add_logit_tables_xml(L203.SubsectorLogit_CHINA, "SubsectorLogit") %>%
      add_xml_data(L203.DeleteSupplysector_CHINA, "DeleteSupplysector") %>%
      add_xml_data(L203.DeleteResTechInput_CHINA, "DeleteResTechInput") %>%
      add_xml_data(L203.SubsectorShrwt_CHINA, "SubsectorShrwt") %>%
      add_xml_data(L203.DeleteSubsector_CHINA, "DeleteSubsector") %>%
      add_xml_data(L203.TechShrwt_CHINA, "TechShrwt") %>%
      add_xml_data(L203.TechCoef_CHINA, "TechCoef") %>%
      add_xml_data(L203.TechPmult_CHINA, "TechPmult") %>%
      add_xml_data(L203.TechDesalCoef_CHINA, "TechCoef") %>%
      add_xml_data(L203.TechDesalShrwt_CHINA, "TechShrwt") %>%
      add_xml_data(L203.TechDesalCost_CHINA, "TechCost") %>%
      add_precursors("L203.Supplysector_CHINA", "L203.SubsectorLogit_CHINA", "L203.DeleteSupplysector_CHINA", "L203.DeleteResTechInput_CHINA", "L203.DeleteSubsector_CHINA", "L203.SubsectorShrwt_CHINA",
                     "L203.TechShrwt_CHINA", "L203.TechCoef_CHINA","L203.TechPmult_CHINA","L203.TechDesalCoef_CHINA", "L203.TechDesalShrwt_CHINA", "L203.TechDesalCost_CHINA") ->
      water_td_CHINA.xml

    return_data(water_td_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
