#' module_gcam.china_batch_electricity_nuclear_CHINA_xml
#'
#' Construct XML data structure for \code{electricity_nuclear_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity_nuclear_CHINA.xml}. The corresponding file in the
#' original data system was \code{L2231.electricity_nuclear_CHINA.R} (gcamchina processing code level2).
module_gcam.china_batch_electricity_nuclear_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2231.StubTechFixOut_elec_nuclear_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electricity_nuclear_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2231.StubTechFixOut_elec_nuclear_CHINA <- get_data(all_data, "L2231.StubTechFixOut_elec_nuclear_CHINA")

    # ===================================================

    # Produce outputs
    create_xml("electricity_nuclear_CHINA.xml") %>%
      add_xml_data(L2231.StubTechFixOut_elec_nuclear_CHINA, "StubTechFixOut") %>%
      add_precursors("L2231.StubTechFixOut_elec_nuclear_CHINA") ->
      electricity_nuclear_CHINA.xml

    return_data(electricity_nuclear_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
