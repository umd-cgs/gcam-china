#' module_gcam.china_LA154.Transport
#'
#' Downscale transportation energy consumption and nonmotor data to the provincial level, generating three ouput tables.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L154.in_EJ_province_trn_m_sz_tech_F}, \code{L154.out_mpkm_province_trn_nonmotor_Yh}, \code{L154.in_EJ_province_trn_F}. The corresponding file in the
#' original data system was \code{LA154.Transport.R} (gcam-china level1).
#' @details Transportation energy data was downscaled in proportion to NBS provincial-level transportation energy data
#' @details Transportation nonmotor data was downscaled in proportion to provincial population
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YangLiu Aug 2018
module_gcam.china_LA154.Transport <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/trnUCD_NBS_mapping",
             "L154.in_EJ_R_trn_m_sz_tech_F_Yh",
             "L154.out_mpkm_R_trn_nonmotor_Yh",
             "L101.Pop_thous_province",
             "L101.inNBS_Mtce_province_S_F",
             "L101.NBS_use_all_Mtce"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L154.in_EJ_province_trn_m_sz_tech_F",
             "L154.out_mpkm_province_trn_nonmotor_Yh",
             "L154.in_EJ_province_trn_F"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    trnUCD_NBS_mapping <- get_data(all_data, "gcam-china/trnUCD_NBS_mapping")
    L154.in_EJ_R_trn_m_sz_tech_F_Yh <- get_data(all_data, "L154.in_EJ_R_trn_m_sz_tech_F_Yh")
    L154.out_mpkm_R_trn_nonmotor_Yh <- get_data(all_data, "L154.out_mpkm_R_trn_nonmotor_Yh")
    L101.Pop_thous_province <- get_data(all_data, "L101.Pop_thous_province")
    L101.inNBS_Mtce_province_S_F <- get_data(all_data, "L101.inNBS_Mtce_province_S_F")
    L101.NBS_use_all_Mtce <- get_data(all_data, "L101.NBS_use_all_Mtce")
    # ===================================================

      # Silence package notes
      GCAM_region_ID <- year <- value <- UCD_sector <- size.class <- UCD_technology <- UCD_fuel <- fuel <- EBProcess <- EBMaterial <-
      fuel_sector <- province <- sector <- value_national <- value_share <- pop <- value_mode <- NULL

      # Calculate the provincial-wise percentages for each of NBS's sector/fuel combinations that is relevant to disaggregating
      # national-level transportation energy to the provinces

      # This starting table is transportation energy consumption by GCAM region (and other variables)
      # We will first subset this data for only China and values that are > 0 in the historical periods

      L154.in_EJ_R_trn_m_sz_tech_F_Yh %>%
        # Drops the years with zero value
        filter(value != 0) %>%
        # Filter for China and for historical years only
        filter(year %in% HISTORICAL_YEARS, GCAM_region_ID == gcamchina.REGION_ID) %>%
        complete(nesting(GCAM_region_ID, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, fuel), year = HISTORICAL_YEARS, fill = list(value = 0)) %>%
        # Fuel and mode will be mapped to NBS fuel and sector
        left_join_error_no_match(trnUCD_NBS_mapping, by = c("fuel", "mode")) ->
        L154.in_EJ_CHINA_trn_m_sz_tech_F_Yh

      # To delete the conflict size class caused by Hong Kong and Macau
      L154.in_EJ_CHINA_trn_m_sz_tech_F_Yh %>%
        mutate(size.class = replace(size.class, mode == "Bus" & size.class == "All", "All"),
               size.class = replace(size.class, mode == "Truck" & size.class == "Truck (0-2t)", "Truck (0-6t)"),
               size.class = replace(size.class, mode == "Truck" & size.class == "Truck (2-5t)", "Truck (0-6t)"),
               size.class = replace(size.class, mode == "Truck" & size.class == "Truck (5-9t)", "Truck (6-14t)"),
               size.class = replace(size.class, mode == "Truck" & size.class == "Truck (9-16t)", "Truck (6-14t)")) %>%
        group_by(GCAM_region_ID, UCD_sector, mode, size.class, UCD_fuel, UCD_technology, fuel, year, EBProcess, EBMaterial) %>%
        summarise(value = sum(value)) %>%
        ungroup ->
        L154.in_EJ_CHINA_trn_m_sz_tech_F_Yh


      # Next, extract the relevant NBS sector & fuel combinations from the full provincial database
      L101.NBS_use_all_Mtce %>%
        # Ensure within historical period
        filter(year %in% HISTORICAL_YEARS) %>%
        # Create concatenated list in base dataframe to match the syntax of our list above
        mutate(fuel_sector = paste(EBProcess, EBMaterial)) %>%
        # Filtering for just NBS-fuel/sector pairs
        filter(fuel_sector %in% paste(L154.in_EJ_CHINA_trn_m_sz_tech_F_Yh$EBProcess, L154.in_EJ_CHINA_trn_m_sz_tech_F_Yh$EBMaterial)) %>%
        select(province, EBProcess, EBMaterial, sector, fuel, year, value) %>%
        group_by(province, year) %>%
        # First zero out NAs in years where some values are NA but not all
        mutate(value = replace(value, is.na(value) & sum(value, na.rm = T) != 0, 0)) %>%
        ungroup %>%
        # use approx_fun rule = 2 to fill out data in years where the entire province is NA
        group_by(province, EBProcess, EBMaterial) %>%
        mutate(value = approx_fun(year, value, rule = 2)) %>%
        ungroup() ->
        L154.NBS_trn_Mtce_province

      # Now the provincial shares can be calculated
      L154.NBS_trn_Mtce_province %>%
        group_by(EBProcess, EBMaterial, year) %>%
        mutate(value_share = value / sum(value, na.rm = T)) %>%
        complete(nesting(year, sector, fuel, EBProcess, EBMaterial), province = gcamchina.PROVINCES_ALL) %>%
        ungroup() %>%
        # NAs were introduced where national values were 0. Replace NAs with zeros.
        replace_na(list(value_share = 0)) %>%
        select(province, EBProcess, EBMaterial, year, value_share) %>%
        left_join(L154.in_EJ_CHINA_trn_m_sz_tech_F_Yh, by = c("EBProcess", "EBMaterial", "year")) %>%
        filter(year %in% HISTORICAL_YEARS) %>%
        mutate(fuel_sector = paste(EBProcess, EBMaterial)) %>%
        replace_na(list(value_share = 0)) %>%
        mutate(value = value * value_share) %>% # Allocating across the provinces
        select(province, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, fuel, year, value) ->
        L154.in_EJ_province_trn_m_sz_tech_F

      # As a final step, aggregate by fuel and name the sector
      # This creates one of three output tables
      L154.in_EJ_province_trn_m_sz_tech_F %>%
        group_by(province, fuel, year) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        # Adding a column named "sector" with "transportation" as the entries
        mutate(sector = "transportation") %>%
        select(province, sector, fuel, year, value) ->
        L154.in_EJ_province_trn_F

      # Apportion non-motorized energy consumption to provinces on the basis of population
      # First we will create the provincial shares based on population
      L101.Pop_thous_province %>%
        complete(province, year = c(1971:2100)) %>%
        group_by(province) %>%
        mutate(pop = approx_fun(year, pop, rule = 2)) %>%
        group_by(year) %>%
        mutate(value_share = pop / sum(pop, na.rm = T)) %>%
        ungroup() %>%
        select(province, year, value_share) %>%
        left_join(L154.out_mpkm_R_trn_nonmotor_Yh %>%
                    rename(value_mode = value) %>%
                    filter(GCAM_region_ID == gcamchina.REGION_ID), by = "year") %>%
        # Apportioning across the modes using the share data
        mutate(value = value_mode * value_share) %>%
        # Ensuring within historical period
        filter(year %in% HISTORICAL_YEARS) %>%
        select(province, mode, year, value) ->
        L154.out_mpkm_province_trn_nonmotor_Yh

    # ===================================================

    L154.in_EJ_province_trn_m_sz_tech_F %>%
      add_title("Transportation energy consumption by province, sector, mode, size class, and fuel") %>%
      add_units("EJ") %>%
      add_comments("Transportation energy consumption data was downscaled to the provincial level using NBS provincial energy data") %>%
      add_legacy_name("L154.in_EJ_province_trn_m_sz_tech_F") %>%
      add_precursors("L154.in_EJ_R_trn_m_sz_tech_F_Yh", "gcam-china/trnUCD_NBS_mapping", "L101.NBS_use_all_Mtce", "L101.inNBS_Mtce_province_S_F") ->
      L154.in_EJ_province_trn_m_sz_tech_F

    L154.out_mpkm_province_trn_nonmotor_Yh %>%
      add_title("Transportation non-motorized travel by mode and province") %>%
      add_units("million person-km") %>%
      add_comments("National data was allocated across provinces in proportion to population") %>%
      add_legacy_name("L154.out_mpkm_province_trn_nonmotor_Yh") %>%
      add_precursors("L154.out_mpkm_R_trn_nonmotor_Yh", "L101.Pop_thous_province") ->
      L154.out_mpkm_province_trn_nonmotor_Yh

    L154.in_EJ_province_trn_F %>%
      add_title("Transportation energy consumption by province and fuel") %>%
      add_units("EJ") %>%
      add_comments("Transportation energy consumption was aggregated by fuel, and the sector was named transportation") %>%
      add_legacy_name("L154.in_EJ_province_trn_F") %>%
      add_precursors("L154.in_EJ_R_trn_m_sz_tech_F_Yh", "gcam-china/trnUCD_NBS_mapping", "L101.NBS_use_all_Mtce", "L101.inNBS_Mtce_province_S_F") ->
      L154.in_EJ_province_trn_F

    return_data(L154.in_EJ_province_trn_m_sz_tech_F, L154.out_mpkm_province_trn_nonmotor_Yh, L154.in_EJ_province_trn_F)
  } else {
    stop("Unknown command")
  }
}
