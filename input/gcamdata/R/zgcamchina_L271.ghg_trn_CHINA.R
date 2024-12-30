# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L271.ghg_trn
#'
#' Non-CO2 GHG emissions parameters for transportation technologies in the China
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L271.ghg_trn_tech_coeff_China}
#' The corresponding file in the original data system was \code{L271.trn_nonCO2_China.R}.
#' @details Prepare level 2 transportation sector non-CO2 GHG emissions files for China.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather
#' @author BY September 2019 / YO 2022 / jiawdo July 2024

  module_gcamchina_L271.ghg_trn <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.en_ghg_emissions",
             "L254.StubTranTechCalInput_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L271.ghg_trn_tech_coeff_CHINA"))
  } else if(command == driver.MAKE) {

    # silence check package notes
    supplysector  <- tranSubsector <- stub.technology <- year <- calibrated.value <-
      region <- Non.CO2 <- emiss.coef <- subsector <- input.emissions <- input.name <- . <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.en_ghg_emissions <- get_data(all_data, "L201.en_ghg_emissions", strip_attributes = TRUE)
    L254.StubTranTechCalInput_CHINA <- get_data(all_data, "L254.StubTranTechCalInput_CHINA", strip_attributes = TRUE)

    # -------------------------------------------------------------------------------------------
    # Future work: once we have province-level nonCO2 GHGs, we will be perform the scaling process by province
    # For this round we will just match the total China emissions

    # transportation energy input
  # transportation energy input
    L254.StubTranTechCalInput_total <- L254.StubTranTechCalInput_CHINA %>%
      group_by(supplysector, tranSubsector, stub.technology, year) %>%
      # use .groups = "drop" to mute a warning message, no change to results
      summarise(calibrated.value = sum(calibrated.value), .groups = "drop") %>%
      ungroup()


    # China total emissions
    L201.en_ghg_emissions_trn <- L201.en_ghg_emissions %>%
      filter(year %in% MODEL_BASE_YEARS & region == gcamchina.REGION & Non.CO2 %in% emissions.GHG_NAMES) %>%
      rename(tranSubsector = subsector) %>%
      # just keep on-road techs
      semi_join(L254.StubTranTechCalInput_total, by = c("supplysector", "tranSubsector", "stub.technology", "year"))

    # develop national average emission factor and map to all provinces
    L201.en_ghg_emissions_trn %>%
      left_join_error_no_match(L254.StubTranTechCalInput_total, by = c("supplysector", "tranSubsector", "stub.technology", "year")) %>%
      mutate(emiss.coef = input.emissions / calibrated.value) %>%
      filter(emiss.coef > 0 & !is.infinite(emiss.coef) & !(is.na(emiss.coef))) %>%
      select(supplysector, tranSubsector, stub.technology, Non.CO2, emiss.coef, year, input.name) %>%
      write_to_all_provinces(gcamchina.PROVINCES_ALL,names = c("region", names(.))) ->
      L271.ghg_trn_tech_coeff_CHINA


    # ===================================================

    # Produce outputs

    L271.ghg_trn_tech_coeff_CHINA %>%
      add_title("Non-CO2 GHG transportation emissions coefficients by province / supplysector / tranSubsector / stub.technology / year / Non.CO2") %>%
      add_units("Tg/million pass-km or Tg/million ton-km") %>%
      add_comments("national average emission factor applied to all provinces") %>%
      add_legacy_name("L271.ghg_trn_tech_coeff_CHINA") %>%
      add_precursors("L201.en_ghg_emissions",
                     "L254.StubTranTechCalInput_CHINA") ->
      L271.ghg_trn_tech_coeff_CHINA


    return_data(L271.ghg_trn_tech_coeff_CHINA)

  } else {
    stop("Unknown command")
  }
}
