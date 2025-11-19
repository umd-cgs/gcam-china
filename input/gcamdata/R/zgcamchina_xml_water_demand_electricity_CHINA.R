# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_batch_water_demand_electricity_xml
#'
#' Construct XML data structure for \code{water_demand_electricity.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{water_demand_electricity_CHINA.xml}.
#' @author Yuqin Li June 2025
module_gcamchina_batch_water_demand_electricity_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/electricity_water_coef_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "water_demand_electricity_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    electricity_water_coef_CHINA <- get_data(all_data, "gcam-china/electricity_water_coef_CHINA")

    # ===================================================

    # Produce outputs
    create_xml("water_demand_electricity_CHINA.xml") %>%
      add_xml_data(electricity_water_coef_CHINA, "TechCoef") %>%
      add_precursors("gcam-china/electricity_water_coef_CHINA") ->
      water_demand_electricity_CHINA.xml

    return_data(water_demand_electricity_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
