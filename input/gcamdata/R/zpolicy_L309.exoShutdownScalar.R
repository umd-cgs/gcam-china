# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_307.exoShutdownScalar
#'
#' Exogenous shutdown decider scalars
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L307.exoShutdownScalar}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_307.exoShutdownScalar <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/A_ExoShutdownScalar"
    ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L307.exoShutdownScalar"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_ExoShutdownScalar <- get_data(all_data, "gcam-china/A_ExoShutdownScalar") %>%
      mutate(xml = if_else(grepl(".xml", xml), xml, paste0(xml, ".xml")))

    # Convert to long and get rid of NA values
    L307.exoShutdownScalar <- A_ExoShutdownScalar %>%
      mutate(exogenous.shutdown.decider = "exogenous.shutdown") %>%
      gather_years(value_col = "output.scalar") %>%
      filter(!is.na(output.scalar))

    # Produce outputs
    L307.exoShutdownScalar %>%
      add_title("Exogenous shutdown decider scalars", overwrite = T) %>%
      add_units("Proportion of vintage") %>%
      add_precursors("gcam-china/A_ExoShutdownScalar") ->
      L307.exoShutdownScalar

    return_data(L307.exoShutdownScalar)
  } else {
    stop("Unknown command")
  }
}

