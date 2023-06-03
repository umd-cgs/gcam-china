# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_batch_interest_rate_CHINA_xml
#'
#' Construct XML data structure for \code{interest_rate_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{interest_rate_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_interest_rate_CHINA.xml} (gcamchina XML).
module_gcamchina_batch_interest_rate_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.InterestRate_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "interest_rate_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.InterestRate_CHINA <- get_data(all_data, "L201.InterestRate_CHINA")

    # ===================================================

    # Produce outputs
    create_xml("interest_rate_CHINA.xml") %>%
      add_xml_data(L201.InterestRate_CHINA, "InterestRate") %>%
      add_precursors("L201.InterestRate_CHINA") ->
      interest_rate_CHINA.xml

    return_data(interest_rate_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
