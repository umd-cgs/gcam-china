# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_batch_electd_CHINA_xml
#'
#' Construct XML data structure for \code{electd_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electd_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_electd_CHINA_xml.R} (gcamchina XML).
module_gcamchina_batch_electd_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L226.DeleteSupplysector_CHINAelec",
             "L226.Supplysector_electd_CHINA",
             "L226.SubsectorLogit_electd_CHINA",
             "L226.SubsectorShrwtFllt_electd_CHINA",
             "L226.SubsectorInterp_electd_CHINA",
             "L226.TechShrwt_electd_CHINA",
             "L226.TechCost_electd_CHINA",
             "L226.TechCoef_electd_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electd_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L226.DeleteSupplysector_CHINAelec <- get_data(all_data, "L226.DeleteSupplysector_CHINAelec")
    L226.Supplysector_electd_CHINA<- get_data(all_data, "L226.Supplysector_electd_CHINA")
    L226.SubsectorLogit_electd_CHINA <- get_data(all_data, "L226.SubsectorLogit_electd_CHINA")
    L226.SubsectorShrwtFllt_electd_CHINA <- get_data(all_data, "L226.SubsectorShrwtFllt_electd_CHINA")
    L226.SubsectorInterp_electd_CHINA <- get_data(all_data, "L226.SubsectorInterp_electd_CHINA")
    L226.TechShrwt_electd_CHINA <- get_data(all_data, "L226.TechShrwt_electd_CHINA")
    L226.TechCost_electd_CHINA <- get_data(all_data, "L226.TechCost_electd_CHINA")
    L226.TechCoef_electd_CHINA <- get_data(all_data, "L226.TechCoef_electd_CHINA")

    # ===================================================

    # Produce outputs
    create_xml("electd_CHINA.xml") %>%
      add_xml_data(L226.DeleteSupplysector_CHINAelec, "DeleteSupplysector") %>%
      add_logit_tables_xml(L226.Supplysector_electd_CHINA, "Supplysector") %>%
      add_logit_tables_xml(L226.SubsectorLogit_electd_CHINA, "SubsectorLogit") %>%
      add_xml_data(L226.SubsectorShrwtFllt_electd_CHINA, "SubsectorShrwtFllt") %>%
      add_xml_data(L226.SubsectorInterp_electd_CHINA, "SubsectorInterp") %>%
      add_xml_data(L226.TechShrwt_electd_CHINA, "TechShrwt") %>%
      add_xml_data(L226.TechCost_electd_CHINA, "TechCost") %>%
      add_xml_data(L226.TechCoef_electd_CHINA, "TechCoef") %>%
      add_precursors("L226.DeleteSupplysector_CHINAelec",
                     "L226.Supplysector_electd_CHINA",
                     "L226.SubsectorLogit_electd_CHINA",
                     "L226.SubsectorShrwtFllt_electd_CHINA",
                     "L226.SubsectorInterp_electd_CHINA",
                     "L226.Supplysector_electd_CHINA",
                     "L226.TechShrwt_electd_CHINA",
                     "L226.TechCost_electd_CHINA",
                     "L226.TechCoef_electd_CHINA") ->
      electd_CHINA.xml

    return_data(electd_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
