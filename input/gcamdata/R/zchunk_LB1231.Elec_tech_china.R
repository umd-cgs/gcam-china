#' module_gcamchina_LB1231.Elec_tech_china
#'
#' Downscaling electricity by province/fuel to province/fuel/technology
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1231.in_EJ_province_elec_F_tech}, \code{L1231.out_EJ_province_elec_F_tech}. The corresponding file in the
#' original data system was \code{LB1231.Elec_tech.R} (gcam-china level1).
#' @details Downscaling electricity by province/fuel to province/fuel/technology
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YangLiu Sep 2018
module_gcamchina_LB1231.Elec_tech_china<- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L123.out_EJ_R_elec_F_Yh",
             "L1231.in_EJ_R_elec_F_tech_Yh",
             "L1231.out_EJ_R_elec_F_tech_Yh",
             "L123.in_EJ_province_elec_F",
             "L123.out_EJ_province_elec_F"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1231.in_EJ_province_elec_F_tech",
             "L1231.out_EJ_province_elec_F_tech"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    year <- value <- GCAM_region_ID <- value.x <- value.y <- province <- sector.x <-
      fuel <- technology <- NULL

    # Load required inputs
    L123.out_EJ_R_elec_F_Yh <- get_data(all_data, "L123.out_EJ_R_elec_F_Yh", strip_attributes = T)
    L1231.in_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.in_EJ_R_elec_F_tech_Yh", strip_attributes = T)
    L1231.out_EJ_R_elec_F_tech_Yh <- get_data(all_data, "L1231.out_EJ_R_elec_F_tech_Yh", strip_attributes = T)
    L123.in_EJ_province_elec_F <- get_data(all_data, "L123.in_EJ_province_elec_F", strip_attributes = T)
    L123.out_EJ_province_elec_F <- get_data(all_data, "L123.out_EJ_province_elec_F", strip_attributes = T)

    # ==================================================
    # Downscaling of electricity by fuel to fuel and technology
    # Computing nation-level shares of technology within fuel
    L1231.out_EJ_R_elec_F_tech_Yh %>%
      filter(GCAM_region_ID == gcamchina.REGION_ID) %>%
      left_join_error_no_match(L123.out_EJ_R_elec_F_Yh, by = c("GCAM_region_ID", "fuel", "year")) %>%
      # Value is equal to technology total / fuel total
      mutate(value = value.x / value.y) %>%
      replace_na(list(value = 0)) %>%
      # Repeat for all provinces
      repeat_add_columns(tibble(province = gcamchina.PROVINCES_ALL)) %>%
      select(province, sector = sector.x, fuel, technology, year, value) ->
      L1231.share_elec_F_tech

    # Multiply the tech shares by the input and output by province and fuel
    L1231.share_elec_F_tech %>%
      # only the fuels that use "inputs" (oil, gas, coal, biomass)
      filter(fuel %in% L1231.in_EJ_R_elec_F_tech_Yh$fuel) %>%
      left_join(L123.in_EJ_province_elec_F, by = c("province", "sector", "fuel", "year")) %>%
      # Province/Technology output = technology share * province/fuel output
      mutate(value = value.x * value.y) %>%
      select(-value.x, - value.y) ->
      L1231.in_EJ_province_elec_F_tech

    L1231.share_elec_F_tech %>%
      # Use left_join because L123.out_EJ_province_elec_F does not contain solar CSP.
      left_join(L123.out_EJ_province_elec_F, by = c("province", "sector", "fuel", "year")) %>%
      replace_na(list(value.y = 0)) %>%
      # Province/Technology output = technology share * province/fuel output
      mutate(value = value.x * value.y) %>%
      select(-value.x, - value.y) ->
      L1231.out_EJ_province_elec_F_tech

    # ===================================================

    # Produce outputs
    # Produce outputs
    L1231.in_EJ_province_elec_F_tech %>%
      add_title("Electricity sector energy consumption by province / fuel / technology") %>%
      add_units("EJ") %>%
      add_comments("National level technology shares multiplied by province level fuel shares") %>%
      add_legacy_name("L1231.in_EJ_province_elec_F_tech") %>%
      add_precursors("L123.out_EJ_R_elec_F_Yh",
                     "L1231.out_EJ_R_elec_F_tech_Yh",
                     "L1231.in_EJ_R_elec_F_tech_Yh",
                     "L123.in_EJ_province_elec_F") ->
      L1231.in_EJ_province_elec_F_tech

    L1231.out_EJ_province_elec_F_tech %>%
      add_title("Electricity generation by province / fuel / technology") %>%
      add_units("EJ") %>%
      add_comments("National level technology shares multiplied by province level fuel shares") %>%
      add_legacy_name("L1231.out_EJ_province_elec_F_tech") %>%
      add_precursors("L123.out_EJ_R_elec_F_Yh",
                     "L1231.out_EJ_R_elec_F_tech_Yh",
                     "L123.out_EJ_province_elec_F") ->
      L1231.out_EJ_province_elec_F_tech

    return_data(L1231.in_EJ_province_elec_F_tech, L1231.out_EJ_province_elec_F_tech)
  } else {
    stop("Unknown command")
  }
}
