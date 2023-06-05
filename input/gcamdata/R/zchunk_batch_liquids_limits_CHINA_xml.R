# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_batch_liquids_limits_china_xml
#'
#' Construct XML data structure for \code{liquids_limits_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{liquids_limits_CHINA.xml}.
module_gcamchina_batch_liquids_limits_china_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( "L270.CreditMkt_CHINA",
              "L270.CreditOutput_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "liquids_limits_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L270.CreditMkt_CHINA <- get_data(all_data, "L270.CreditMkt_CHINA")
    L270.CreditOutput_CHINA <- get_data(all_data, "L270.CreditOutput_CHINA")

    # ===================================================

    # Produce outputs
    create_xml("liquids_limits_CHINA.xml") %>%
      add_xml_data(L270.CreditMkt_CHINA, "PortfolioStd") %>%
      add_xml_data(L270.CreditOutput_CHINA, "GlobalTechRESSecOut") %>%
      add_precursors("L270.CreditMkt_CHINA",
                     "L270.CreditOutput_CHINA") ->
      liquids_limits_CHINA.xml

    return_data(liquids_limits_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
