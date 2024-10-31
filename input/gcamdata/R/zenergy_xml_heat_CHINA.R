# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_heat_china_xml
#'
#' Construct XML data structure for \code{heat.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{heat_china.xml}. The corresponding file in the
#' original data system was \code{batch_heat.xml.R} (energy XML).
module_energy_heat_china_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L224.Supplysector_heat_china",
             "L224.SubsectorLogit_heat_china",
             "L224.SubsectorShrwtFllt_heat_china",
             "L224.SubsectorInterp_heat_china",
             "L224.StubTech_heat_china",
             "L224.StubTechCalInput_heat_china",
             "L224.StubTechSecOut_elec_china",
             "L224.StubTechCost_elec_china",
             "L224.DeleteSupplysector_CHINAheat",
             "L224.StubTechMarket_heat_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "heat_china.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L224.Supplysector_heat_china <- get_data(all_data, "L224.Supplysector_heat_china")
    L224.SubsectorLogit_heat_china <- get_data(all_data, "L224.SubsectorLogit_heat_china")
    L224.SubsectorShrwtFllt_heat_china <- get_data(all_data, "L224.SubsectorShrwtFllt_heat_china")
    L224.SubsectorInterp_heat_china <- get_data(all_data, "L224.SubsectorInterp_heat_china")
    L224.StubTech_heat_china <- get_data(all_data, "L224.StubTech_heat_china")
    L224.StubTechCalInput_heat_china <- get_data(all_data, "L224.StubTechCalInput_heat_china")
    L224.StubTechSecOut_elec_china <- get_data(all_data, "L224.StubTechSecOut_elec_china")
    L224.StubTechCost_elec_china <- get_data(all_data, "L224.StubTechCost_elec_china")
    L224.DeleteSupplysector_CHINAheat <- get_data(all_data, "L224.DeleteSupplysector_CHINAheat")
    L224.StubTechMarket_heat_CHINA <- get_data(all_data, "L224.StubTechMarket_heat_CHINA")

    share.weight <- year.share.weight <- NULL # Silence package checks

    # ===================================================

    # Produce outputs
    create_xml("heat_china.xml") %>%
      add_logit_tables_xml(L224.Supplysector_heat_china, "Supplysector") %>%
      add_logit_tables_xml(L224.SubsectorLogit_heat_china, "SubsectorLogit") %>%
      add_xml_data(L224.SubsectorShrwtFllt_heat_china, "SubsectorShrwtFllt") %>%
      add_xml_data(L224.SubsectorInterp_heat_china, "SubsectorInterp") %>%
      add_xml_data(L224.StubTech_heat_china, "StubTech") %>%
      add_xml_data(L224.StubTechCalInput_heat_china, "StubTechCalInput") %>%
      add_xml_data(L224.StubTechSecOut_elec_china, "StubTechSecOut") %>%
      add_xml_data(L224.DeleteSupplysector_CHINAheat, "DeleteSupplysector") %>%
      add_xml_data(L224.StubTechCost_elec_china, "StubTechCost") %>%
      add_xml_data(L224.StubTechMarket_heat_CHINA, "StubTechMarket") %>%
      add_precursors("L224.Supplysector_heat_china",
                     "L224.SubsectorLogit_heat_china",
                     "L224.SubsectorShrwtFllt_heat_china",
                     "L224.SubsectorInterp_heat_china",
                     "L224.StubTech_heat_china",
                     "L224.StubTechCalInput_heat_china",
                     "L224.StubTechSecOut_elec_china",
                     "L224.StubTechCost_elec_china",
                     "L224.DeleteSupplysector_CHINAheat",
                     "L224.StubTechMarket_heat_CHINA") ->
      heat_china.xml

    return_data(heat_china.xml)
  } else {
    stop("Unknown command")
  }
}
