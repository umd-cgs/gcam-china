# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_HDDCDD_constdds_xml
#'
#' Construct XML data structure for \code{HDDCDD_constdds_China.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{HDDCDD_constdds_China.xml}.
module_gcamchina_HDDCDD_constdds_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L244.HDDCDD_constdds_China",
             "L244.DeleteThermalService_gcamchina"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "HDDCDD_constdds_China.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.HDDCDD_constdds_China <- get_data(all_data, "L244.HDDCDD_constdds_China")
    L244.DeleteThermalService_gcamchina <- get_data(all_data, "L244.DeleteThermalService_gcamchina")

    L244.HDDCDD_constdds_China <- L244.HDDCDD_constdds_China %>%
      anti_join(L244.DeleteThermalService_gcamchina %>%
                  select(region,gcam.consumer,thermal.building.service.input),
                by = c("region","gcam.consumer","thermal.building.service.input"))

    # Produce outputs
    create_xml("HDDCDD_constdds_China.xml") %>%
      add_xml_data(L244.HDDCDD_constdds_China, "HDDCDD") %>%
      add_precursors("L244.HDDCDD_constdds_China","L244.DeleteThermalService_gcamchina") ->
      HDDCDD_constdds_China.xml

    return_data(HDDCDD_constdds_China.xml)
  } else {
    stop("Unknown command")
  }
}
