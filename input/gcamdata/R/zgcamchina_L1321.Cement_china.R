# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L1321.Cement
#'
#' To calculate national cement production, energy inputs and Input-output coefficients to provinces
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1321.out_Mt_province_cement_Yh}, \code{L1321.IO_GJkg_province_cement_F_Yh}, \code{L1321.in_EJ_province_cement_F_Y}. The corresponding file in the
#' original data system was \code{LA1321.Cement.R} (gcam-china level1).
#' @details The tables for cement production, i.e., out, and energy inputs, i.e., in, were calculated by applying province shares to national data.
#' @details The province shares were determined by the provinces' relative values of cement shipments.
#' @details The input-out coefficients were downscaled to the provinces in proportion to the national data.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YangLiu Aug 2018 / YangOu Dec 2023
module_gcamchina_L1321.Cement <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L101.inNBS_Mtce_province_S_F",
             "L1321.out_Mt_R_cement_Yh",
             "L1321.IO_GJkg_R_cement_F_Yh",
             "L1321.in_EJ_R_cement_F_Y"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1321.out_Mt_province_cement_Yh",
             "L1321.IO_GJkg_province_cement_F_Yh",
             "L1321.in_EJ_province_cement_F_Y"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    sector <- fuel <- province <- multiplier <- GCAM_region_ID <- year <- value <- NULL   # silence package check notes

    # Load required inputs
    L1321.out_Mt_R_cement_Yh <- get_data(all_data, "L1321.out_Mt_R_cement_Yh")
    L1321.IO_GJkg_R_cement_F_Yh <- get_data(all_data, "L1321.IO_GJkg_R_cement_F_Yh", strip_attributes = T)
    L1321.in_EJ_R_cement_F_Y <- get_data(all_data, "L1321.in_EJ_R_cement_F_Y")

    # Use industrial coal to downscale cement to provinces
    L101.inNBS_Mtce_province_S_F <- get_data(all_data, "L101.inNBS_Mtce_province_S_F")

    # ===================================================
    # Assigning national cement production to provinces
    L101.inNBS_Mtce_province_S_F %>%
      filter(sector == "industry" & fuel == "coal") %>%
      group_by(year) %>%
      mutate(multiplier = value / sum(value)) %>%
      group_by(province) %>%
      mutate(multiplier = approx_fun(year, multiplier, rule = 2), sector = "cement") %>%
      ungroup() %>%
      select(-value, -fuel) ->
      L1321.in_pct_province_cement

    # To calculate province-level data by multiplying the province share by the china component in the global data
    # To generate cement production by province / historical year
    L1321.in_pct_province_cement %>%
      left_join_error_no_match(L1321.out_Mt_R_cement_Yh %>% filter(GCAM_region_ID == gcamchina.REGION_ID), by = c("year", "sector")) %>%
      mutate(value = value * multiplier) %>% # Multiplying the national amount with the province share
      select(province, sector, year, value) ->
      L1321.out_Mt_province_cement_Yh

    # To generate Input-output coefficients of cement production by province / fuel / historical year
    L1321.IO_GJkg_R_cement_F_Yh %>%
      filter(GCAM_region_ID == gcamchina.REGION_ID) %>%
      repeat_add_columns(tibble(province = unique(L1321.out_Mt_province_cement_Yh$province))) %>%
      select(province, sector, fuel, year, value) ->
      L1321.IO_GJkg_province_cement_F_Yh

    # Note that this assumes the same fuel blend in all provinces
    # To generate Energy inputs to cement production by province / fuel / historical year
    L1321.in_pct_province_cement %>%
      repeat_add_columns(tibble(fuel = unique(L1321.in_EJ_R_cement_F_Y$fuel))) %>%
      left_join_error_no_match(L1321.in_EJ_R_cement_F_Y %>% filter(GCAM_region_ID == gcamchina.REGION_ID), by = c("year", "sector", "fuel")) %>%
      mutate(value = multiplier * value) %>%
      select(province, sector, fuel, year, value) ->
      L1321.in_EJ_province_cement_F_Y

    # ===================================================

    L1321.out_Mt_province_cement_Yh %>%
      add_title("Cement production by province / historical year") %>%
      add_units("Mt") %>%
      add_comments("downscaling national data using province shares") %>%
      add_comments("these province shares were calculated to be proportional to the their values of cement shipments") %>%
      add_legacy_name("L1321.out_Mt_province_cement_Yh") %>%
      add_precursors("L101.inNBS_Mtce_province_S_F", "L1321.out_Mt_R_cement_Yh") ->
      L1321.out_Mt_province_cement_Yh

    L1321.IO_GJkg_province_cement_F_Yh %>%
      add_title("Input-output coefficients of cement production by province / input / historical year") %>%
      add_units("GJ/kg and kg/kg") %>%
      add_comments("downscaling national data assuming the same IO coefficients for each respective fuel") %>%
      add_legacy_name("L1321.IO_GJkg_province_cement_F_Yh") %>%
      add_precursors("L101.inNBS_Mtce_province_S_F", "L1321.IO_GJkg_R_cement_F_Yh") ->
      L1321.IO_GJkg_province_cement_F_Yh

    L1321.in_EJ_province_cement_F_Y %>%
      add_title("Energy inputs to cement production by province / fuel / historical year") %>%
      add_units("EJ/yr") %>%
      add_comments("downscaling national data using province shares") %>%
      add_comments("these province shares were calculated to be proportional to the their values of cement shipments") %>%
      add_legacy_name("L1321.in_EJ_province_cement_F_Y") %>%
      add_precursors("L101.inNBS_Mtce_province_S_F", "L1321.in_EJ_R_cement_F_Y") ->
      L1321.in_EJ_province_cement_F_Y

    return_data(L1321.out_Mt_province_cement_Yh, L1321.IO_GJkg_province_cement_F_Yh, L1321.in_EJ_province_cement_F_Y)
  } else {
    stop("Unknown command")
  }
}
