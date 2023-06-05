#' module_gcam.china_LA101.Energy_Balance
#'
#' This chunk generates the historical energy data by province
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.NBS_use_all_Mtce}, \code{L101.inNBS_Mtce_province_S_F}. The corresponding file in the
#' original data system was \code{LA101.Energy_Balance.R} (gcam-china level1).
#' @details This chunk processes historical energy data by province, interpolate missinge value
#' and then estimates of Tibet shares of energy use by sector.  A rough estimate to give scale
#' Tibet accounts for 1% of the national energy
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread fill
#' @author Yang July 2018

module_gcam.china_LA101.Energy_Balance <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "gcam-china/NBS_CESY_process",
             FILE = "gcam-china/NBS_CESY_material",
             FILE = "gcam-china/en_balance_Mtce_Yh_province",
		  	 FILE = "gcam-china/Tibet_share",
			 FILE = "gcam-china/tibet_shares_mappings"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L101.NBS_use_all_Mtce",
             "L101.inNBS_Mtce_province_S_F"))
  } else if(command == driver.MAKE) {

    share <- year <- sector <- fuel <- province <- pop <- share <- province.name <-
      EBProcess <- EBMaterial <- value <- org <- xz.sector <- xz.fuel <- NULL# silence package check.

  all_data <- list(...)[[1]]

    # Load required inputs
     province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = T)
     NBS_CESY_process <- get_data(all_data, "gcam-china/NBS_CESY_process", strip_attributes = T)
     NBS_CESY_material <- get_data(all_data, "gcam-china/NBS_CESY_material", strip_attributes = T)
     en_balance_Mtce_Yh_province <- get_data(all_data, "gcam-china/en_balance_Mtce_Yh_province", strip_attributes = T)
     Tibet_share  <- get_data(all_data, "gcam-china/Tibet_share", strip_attributes = T)
     tibet_shares_mappings <- get_data(all_data, "gcam-china/tibet_shares_mappings", strip_attributes = T)

    # Perform computations

    # Removing the whole China category, and matching in intermediate fuel and sector names
    # The energy balance calls Tibet by it's alternative name Xizang, we will need to switch
    # it for the mappings to work
    en_balance_Mtce_Yh_province %>%
      filter(province.name != "China" & year %in% HISTORICAL_YEARS) %>%
      mutate(province.name = replace(province.name, province.name == "Xizang", "Tibet")) %>%
      map_province_name(province_names_mappings, "province", TRUE) %>%
      # There are NAs in NBS_CESY_process and NBS_CESY_material, so we use left_join.
      left_join(NBS_CESY_process, by = "EBProcess") %>%
      left_join(NBS_CESY_material, by = "EBMaterial") ->
      L101.NBS_use_all_Mtce

    # Aggregating NBS province energy data by GCAM sector and fuel
    L101.NBS_use_all_Mtce %>%
      filter(!is.na(sector) & !is.na(fuel)) %>%
      group_by(province, year, sector, fuel) %>%
      # Drop all the NAs, so they do not generate zero values when all observations are missing for certain groups
      filter(!is.na(value)) %>%
      summarise(value = sum(value)) %>%
      ungroup() ->
      L101.inNBS_Mtce_province_S_F


    # Interpolate missing values where possible (rule=1)
    L101.NBS_use_all_Mtce %>%
      complete(nesting(province, EBProcess, EBMaterial, fuel, sector), year = HISTORICAL_YEARS) %>%
      arrange(province, year) %>%
      group_by(province, fuel, sector, EBProcess, EBMaterial) %>%
      mutate(value = as.numeric(value), value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      # When there is one data point, approx_fun replaced it with NA, so we need to add those data back
      left_join(L101.NBS_use_all_Mtce %>% rename(org = value),
                by = c("province", "EBProcess", "EBMaterial", "fuel", "sector", "year")) %>%
      mutate(value = replace(value, is.na(value), org[is.na(value)])) %>%
      select(-org) ->
      L101.NBS_use_all_Mtce

    # Interpolate missinge values where possible (rule=1)
    L101.inNBS_Mtce_province_S_F %>%
      complete(nesting(province, sector, fuel), year = HISTORICAL_YEARS) %>%
      arrange(province, year) %>%
      group_by(province, fuel, sector) %>%
      mutate(value = approx_fun(year, value, rule = 1)) %>%
      ungroup() %>%
      # When there is one data point, approx_fun replaced it with NA, so we need to add those data back
      left_join(L101.inNBS_Mtce_province_S_F %>% rename(org = value),
                by = c("province", "fuel", "sector", "year")) %>%
      mutate(value = replace(value, is.na(value), org[is.na(value)])) %>%
      select(-org) ->
      L101.inNBS_Mtce_province_S_F

    # Make adjustments to  Tibet (XZ) which is mostly unrepresented in the CESY
    # We have estimates of Tibet shares of energy use by sector.  A rough estimate to give scale
    # Tibet accounts for 1% of the national energy.

    # Calculate national total first
    L101.inNBS_Mtce_province_S_F %>%
      group_by(year) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup() ->
      L101.inNBS_Mtce_CHINA

    Tibet_share %>%
      left_join(tibet_shares_mappings, by = c("xz.sector", "xz.fuel")) %>%
      # We are removing biomass since all the other provinces do not have it
      filter(!grepl('biomass', fuel)) %>%
      select(-xz.sector, -xz.fuel) %>%
      repeat_add_columns(tibble(year = HISTORICAL_YEARS)) %>%
      # There are NAs in the raw data, so we use left_join.
      left_join(L101.inNBS_Mtce_CHINA, by = "year") %>%
      mutate(value = value * gcamchina.TIBET_NATIONAL_ENERGY_SHARE * share, province = "XZ") %>%
      select(-share) ->
      L101.NBS_Mtce_tibet_S_F

    L101.NBS_Mtce_tibet_S_F %>%
      bind_rows(L101.NBS_use_all_Mtce %>% filter(province != "XZ")) %>%
      # In the test, if the first data is NA, it will be considered that the type of the entire column is a character, and a type error occurs.
      arrange(desc(year)) ->
      L101.NBS_use_all_Mtce

    L101.NBS_Mtce_tibet_S_F %>%
      group_by(province, year, sector, fuel) %>%
	    summarise(value = sum(value, na.rm = T)) %>%
      ungroup() %>%
      bind_rows(L101.inNBS_Mtce_province_S_F %>% filter(province != "XZ")) ->
      L101.inNBS_Mtce_province_S_F

    # Produce outputs
    L101.NBS_use_all_Mtce %>%
      add_title("NBS china energy statistical yearbook by sector / fuel / year") %>%
      add_units("Unit = Mtce") %>%
      add_comments("Missing values are interpolated if bounded and filled with the next closest value otherwise") %>%
      add_comments("Make adjustments to  Tibet (XZ) and suppose tibet accounts for 1% of the national energy. ") %>%
      add_legacy_name("L101.NBS_use_all_Mtce") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/NBS_CESY_process",
                     "gcam-china/NBS_CESY_material",
                     "gcam-china/en_balance_Mtce_Yh_province",
                     "gcam-china/Tibet_share",
                     "gcam-china/tibet_shares_mappings") ->
      L101.NBS_use_all_Mtce

    L101.inNBS_Mtce_province_S_F %>%
      add_title("NBS china energy statistical yearbook by GCAM sector / fuel / year") %>%
      add_units("Unit = Mtce") %>%
      add_comments("Missing values are interpolated if bounded and filled with the next closest value otherwise") %>%
      add_comments("Make adjustments to Tibet (XZ) and suppose tibet accounts for 1% of the national energy. ") %>%
      add_legacy_name("L101.inNBS_Mtce_province_S_F") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/NBS_CESY_process",
                     "gcam-china/NBS_CESY_material",
                     "gcam-china/en_balance_Mtce_Yh_province",
                     "gcam-china/Tibet_share",
                     "gcam-china/tibet_shares_mappings") ->
      L101.inNBS_Mtce_province_S_F

    return_data(L101.NBS_use_all_Mtce, L101.inNBS_Mtce_province_S_F)
  } else {
    stop("Unknown command")
  }
}
