# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_LB1232.Elec_subregions_china
#'
#' Aggregates China province electricity generation to electricity subregions.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1232.out_EJ_sR_elec_CHINA}. The corresponding file in the
#' original data system was \code{LB1232.Elec_subregions.R} (gcam-china level1).
#' @details Aggregates CHINA province electricity generation to electricity subregions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YangLiu September 2018 / YangOu December 2023
module_gcamchina_LB1232.Elec_subregions_china <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             "L1231.out_EJ_province_elec_F_tech"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1232.out_EJ_sR_elec_CHINA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    province <- grid.region <- year <- value <- sector <- NULL

    # Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings") %>%
      select(province, grid.region)
    L1231.out_EJ_province_elec_F_tech <- get_data(all_data, "L1231.out_EJ_province_elec_F_tech", strip_attributes = T)

    # ===================================================
    # Aggregating provinces to electricity subregions
    L1232.out_EJ_sR_elec_CHINA <- L1231.out_EJ_province_elec_F_tech %>%
      left_join(province_names_mappings, by = "province") %>%
      group_by(grid.region, sector, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      # ===================================================
    # Produce outputs
    add_title("Electricity generation by grid region/fuel/technology") %>%
      add_units("EJ") %>%
      add_comments("L1231.out_EJ_province_elec_F_tech aggregated to FERC region") %>%
      add_legacy_name("L1232.out_EJ_sR_elec_CHINA") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "L1231.out_EJ_province_elec_F_tech")

    return_data(L1232.out_EJ_sR_elec_CHINA)
  } else {
    stop("Unknown command")
  }
}
