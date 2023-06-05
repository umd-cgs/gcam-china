#' module_gcam.china_LA1322.Fert
#'
#' Calculate input-output intensity coefficients and input energy for province fertilizer production
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1322.out_Mt_province_Fert_Yh}, \code{L1322.IO_GJkg_province_Fert_F_Yh}, \code{L1322.in_EJ_province_Fert_Yh}. The corresponding file in the
#' original data system was \code{LA1322.Fert.R} (gcam-china level1).
#' @details Calculate input-output intensity coefficients and input energy for province fertilizer production from province shares of national values.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YangLiu Aug 2018
module_gcam.china_LA1322.Fert <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L101.NBS_use_all_Mtce",
             "L1322.Fert_Prod_MtN_R_F_Y",
             "L1322.IO_R_Fert_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1322.out_Mt_province_Fert_Yh",
             "L1322.IO_GJkg_province_Fert_F_Yh",
             "L1322.in_EJ_province_Fert_Yh"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L1322.Fert_Prod_MtN_R_F_Y <- get_data(all_data, "L1322.Fert_Prod_MtN_R_F_Y")
    L1322.IO_R_Fert_F_Yh <- get_data(all_data, "L1322.IO_R_Fert_F_Yh", strip_attributes = T)
    # Proxy for downscaling fertilizer to provinces
    # TODO: Find a better proxy
    L101.NBS_use_all_Mtce <- get_data(all_data, "L101.NBS_use_all_Mtce")

    # Silence package check
    year <- GCAM_region_ID <- value <- org <- sum <- province <-
      sector <- fuel <- EBProcess <- EBMaterial <- multiplier <- output <- . <- NULL

    # ===================================================
    # Get province fertilizer production, input-output intensity coefficients, and
    # input energy for fertilizer production from national values.

    # Assigning national fertilizer production to provinces
    L101.NBS_use_all_Mtce %>%
      filter(EBProcess == "Agriculture", EBMaterial == "Coal Raw") %>%
      # First zero out NAs in years where some values are NA but not all
      group_by(province, year) %>%
      mutate(value = replace(value, is.na(value) & sum(value, na.rm = T) != 0, 0)) %>%
      ungroup ->
      L1322.in_Mtce_province_Fert

    L1322.in_Mtce_province_Fert %>%
      group_by(province, sector, fuel) %>%
      # use approx_fun rule = 2 to fill out data in years where the entire province is NA
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup %>%
      # When there is one data point, approx_fun replaced it with NA, but what we needed is to replace all NAs with that data.
      left_join(L1322.in_Mtce_province_Fert %>% rename(org = value),
                by = c("province", "fuel", "sector", "year", "EBProcess", "EBMaterial")) %>%
      group_by(sector, fuel, province) %>%
      # Only under the occasion when there is one data point in the group,
      # approx_fun replaces the one value with NA.
      # The original gcam_interp function replaces all missing values in that group with the one value when rule = 2.
      mutate(value = replace(value, is.na(value) & sum(org[is.na(value)], na.rm = T) != 0,
                             sum(org[is.na(value)], na.rm = T))) %>%
      select(-org) %>%
      ungroup ->
      L1322.in_Mtce_province_Fert

    # Convert to province precentages

    L1322.in_Mtce_province_Fert %>%
      group_by(sector, fuel, year) %>%
      mutate(multiplier = value / sum(value)) %>%
      ungroup %>%
      group_by(province, fuel) %>%
      mutate(multiplier = approx_fun(year, multiplier, rule = 2), sector = "N fertilizer") %>%
      ungroup() %>%
      select(-value, -EBProcess, -EBMaterial, -fuel) %>%
      # Use the same shares for all fuels
      repeat_add_columns(tibble(fuel = unique(L1322.Fert_Prod_MtN_R_F_Y$fuel))) ->
      L1322.in_pct_province_Fert_repF

    # Approtion national production to provinces use "Coal Raw" in Agriculture as a proxy
    L1322.in_pct_province_Fert_repF %>%
      left_join_error_no_match(filter(L1322.Fert_Prod_MtN_R_F_Y, GCAM_region_ID == gcamchina.REGION_ID), by = c("fuel", "sector", "year")) %>%
      mutate(value = value * multiplier) %>% # Multiplying the national amount with the province share
      select(province, sector, year, value, fuel) ->
      L1322.out_Mt_province_Fert_Yh


    # Assuming all provinces have the same IO coefficients
    L1322.IO_R_Fert_F_Yh %>%
      filter(GCAM_region_ID == gcamchina.REGION_ID) %>%
      repeat_add_columns(tibble(province = gcamchina.PROVINCES_NOHKMC)) %>%
      filter(province != 'XZ') %>%
      select(province, sector, fuel, year, value) ->
      L1322.IO_GJkg_province_Fert_F_Yh

    L1322.IO_GJkg_province_Fert_F_Yh %>%
      left_join_error_no_match(L1322.out_Mt_province_Fert_Yh %>% rename(output = value), by = c("province", "year", "sector", "fuel")) %>%
      # Calculate input by multiplying output with input-output coefficients
      mutate(value = value * output, output = NULL) ->
      L1322.in_EJ_province_Fert_Yh

    # ===================================================

    # Produce outputs
    L1322.out_Mt_province_Fert_Yh %>%
      add_title("Fert production by province / historical year") %>%
      add_units("Unit = Mt)") %>%
      add_comments("Scaled the China national fertilizer production by the province's share of fertilizer production based on NBS") %>%
      add_legacy_name("L1322.out_Mt_province_Fert_Yh") %>%
      add_precursors("L1322.Fert_Prod_MtN_R_F_Y", "L101.NBS_use_all_Mtce", "L1322.IO_R_Fert_F_Yh") ->
      L1322.out_Mt_province_Fert_Yh

    L1322.IO_GJkg_province_Fert_F_Yh %>%
      add_title("Input-output coefficients of Fert production by province / input / historical year") %>%
      add_units("Unit = GJ/kg and kg/kg") %>%
      add_comments("All provinces are assumed to have the same fertilizer IO coefficient as the national value") %>%
      add_legacy_name("L1322.IO_GJkg_province_Fert_F_Yh") %>%
      add_precursors("L1322.Fert_Prod_MtN_R_F_Y", "L101.NBS_use_all_Mtce", "L1322.IO_R_Fert_F_Yh") ->
      L1322.IO_GJkg_province_Fert_F_Yh

    L1322.in_EJ_province_Fert_Yh %>%
      add_title("Energy inputs to Fertilizer production by province / input / historical year") %>%
      add_units("Unit = GJ/kg and kg/kg") %>%
      add_comments("Provincial fertilizer production multiplied by provincial fertilizer input-output coefficient") %>%
      add_legacy_name("L1322.in_EJ_province_Fert_Yh") %>%
      add_precursors("L1322.Fert_Prod_MtN_R_F_Y", "L101.NBS_use_all_Mtce", "L1322.IO_R_Fert_F_Yh") ->
      L1322.in_EJ_province_Fert_Yh

    return_data(L1322.out_Mt_province_Fert_Yh, L1322.IO_GJkg_province_Fert_F_Yh, L1322.in_EJ_province_Fert_Yh)
  } else {
    stop("Unknown command")
  }
}
