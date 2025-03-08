# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_trn_ghg_emissions_xml
#'
#' Construct XML data structure for \code{othertrn_emissions_China.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{trn_ghg_emissions_China.xml}. The corresponding file in the
#' original data system was \code{batch_transport_emissions_China_xml.R} (gcamchina XML).
  module_gcamchina_trn_ghg_emissions_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L271.ghg_trn_tech_coeff_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "trn_ghg_emissions_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L271.ghg_trn_tech_coeff_CHINA<- get_data(all_data, "L271.ghg_trn_tech_coeff_CHINA")

    # ===================================================

    # Produce outputs
    create_xml("trn_ghg_emissions_CHINA.xml") %>%
      add_xml_data(L271.ghg_trn_tech_coeff_CHINA, "TrnInputEmissCoeff") %>%
      add_precursors("L271.ghg_trn_tech_coeff_CHINA") ->
      trn_ghg_emissions_CHINA.xml

    return_data(trn_ghg_emissions_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
