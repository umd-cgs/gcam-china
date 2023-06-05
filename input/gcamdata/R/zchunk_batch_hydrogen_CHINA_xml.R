# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_batch_hydrogen_CHINA_xml
#'
#' Construct XML data structure for \code{hydrogen_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{hydrogen_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_hydrogen_CHINA_xml.R} (gcamchina XML).
module_gcamchina_batch_hydrogen_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L225.DeleteSubsector_h2_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "hydrogen_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L225.DeleteSubsector_h2_CHINA <- get_data(all_data, "L225.DeleteSubsector_h2_CHINA")

    # ===================================================

    # Produce outputs
    create_xml("hydrogen_CHINA.xml") %>%
      add_xml_data(L225.DeleteSubsector_h2_CHINA, "DeleteSubsector") %>%
      add_precursors("L225.DeleteSubsector_h2_CHINA") ->
      hydrogen_CHINA.xml

    return_data(hydrogen_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
