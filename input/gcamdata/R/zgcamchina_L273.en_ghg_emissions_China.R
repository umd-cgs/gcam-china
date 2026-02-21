# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#'  module_gcamchina_L273.en_ghg_emissions
#'
#' Define non-CO2 GHG emissions for GCAM-China provinces, including 1. CH4 and N2O in refinery,
#' buildings, N fertilizer, industrial energy use;
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L273.en_ghg_tech_coeff_CHINA}, \code{L273.en_ghg_emissions_CHINA}, \code{L273.out_ghg_emissions_CHINA},
#' and \code{L273.MAC_higwp_CHINA}. The corresponding file in the
#' original data system was \code{L273.en_ghg_emissions_CHINA.R}.
#' @details KALYN MTB YO
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author KRD 2018; MTB 2018; YO 2021 ; jiawdo July 2024

  module_gcamchina_L273.en_ghg_emissions <- function(command, ...) {
  UCD_tech_map_name <- if_else(energy.TRAN_UCD_MODE == 'rev.mode',
                               "energy/mappings/UCD_techs_revised", "energy/mappings/UCD_techs")
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             "L123.out_EJ_province_ownuse_elec",
             "L1322.in_EJ_province_Fert_Yh",
             "L201.en_ghg_emissions",
             "L241.nonco2_tech_coeff",
             "L241.hfc_all",
             "L241.pfc_all",
             "L252.MAC_higwp",
             "L252.MAC_higwp_tc_average",
             "L252.MAC_higwp_phaseInTime",
             "L222.StubTech_en_CHINA",
             "L232.StubTechCalInput_indenergy_CHINA",
             "L244.StubTechCalInput_bld_gcamchina",
             "L244.GlobalTechEff_bld_gcamchina",
             # the following files to be able to map in the input.name to
             # use for the input-driver
             FILE = "energy/A22.globaltech_input_driver",
             FILE = "energy/A23.globaltech_input_driver",
             FILE = "energy/A25.globaltech_input_driver",
             # the following to be able to map in the input.name to
             # use for the input-driver for res + ind
             FILE = "energy/calibrated_techs",
             FILE = "gcam-china/calibrated_techs_bld_china",
             FILE = UCD_tech_map_name))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L273.en_ghg_tech_coeff_CHINA",
             "L273.en_ghg_emissions_CHINA",
             "L273.out_ghg_emissions_CHINA",
             "L273.MAC_higwp_CHINA",
             "L273.MAC_higwp_TC_CHINA",
             "L273.MAC_higwp_phaseInTime_CHINA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    #Silence package checks
    #grid_regon - grid.region
    CH4 <- GCAM_region_ID <- N2O <- Non.CO2 <- calibrated.value <- calibrated.value.x <-
      calibrated.value.y <- depresource <- efficiency <- elec_technology <- emiss.coef <-
      emiss.coeff <- fuel <- fuel_input <- fuel_input_share <- grid.region <-
      input.emissions <- keep <- mac.control <- mac.reduction <- market.name <-
      output.emissions <- palette <- region <- sector <- service_output <- service_output2 <-
      share <- province <- province_technology <- stub.technology <- subsector <- supplysector <-
      tax <- technology <- value <- value2 <- year <- minicam.energy.input <- tranSubsector <-
      tranTechnology <- tech.change.year <- tech.change <- mac.phase.in.time <- NULL

    # Load required inputs
    province_subregions <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = TRUE)
    L123.out_EJ_province_ownuse_elec <- get_data(all_data, "L123.out_EJ_province_ownuse_elec", strip_attributes = TRUE)
    L1322.in_EJ_province_Fert_Yh <- get_data(all_data, "L1322.in_EJ_province_Fert_Yh", strip_attributes = TRUE)
    L201.en_ghg_emissions <- get_data(all_data, "L201.en_ghg_emissions", strip_attributes = TRUE)
    L241.nonco2_tech_coeff <- get_data(all_data, "L241.nonco2_tech_coeff", strip_attributes = TRUE)
    L241.hfc_all <- get_data(all_data, "L241.hfc_all", strip_attributes = TRUE)
    L241.pfc_all <- get_data(all_data, "L241.pfc_all", strip_attributes = TRUE)
    L252.MAC_higwp <- get_data(all_data, "L252.MAC_higwp", strip_attributes = TRUE)
    L252.MAC_higwp_tc_average <- get_data(all_data, "L252.MAC_higwp_tc_average", strip_attributes = TRUE)
    L252.MAC_higwp_phaseInTime <- get_data(all_data, "L252.MAC_higwp_phaseInTime", strip_attributes = TRUE)
    L222.StubTech_en_CHINA <- get_data(all_data, "L222.StubTech_en_CHINA", strip_attributes = TRUE)
    L232.StubTechCalInput_indenergy_CHINA <- get_data(all_data, "L232.StubTechCalInput_indenergy_CHINA", strip_attributes = TRUE)
    L244.StubTechCalInput_bld_gcamchina <- get_data(all_data, "L244.StubTechCalInput_bld_gcamchina", strip_attributes = TRUE)
    L244.GlobalTechEff_bld_gcamchina <- get_data(all_data, "L244.GlobalTechEff_bld_gcamchina", strip_attributes = TRUE)

    # Align Chinese subsector emissions in GCAM-global with those in GCAM-China
    # make a complete mapping to be able to look up with sector + subsector + tech the
    # input name to use for an input-driver
    bind_rows(
      get_data(all_data, "energy/A22.globaltech_input_driver"),
      get_data(all_data, "energy/A23.globaltech_input_driver"),
      get_data(all_data, "energy/A25.globaltech_input_driver")
    ) %>%
      rename(stub.technology = technology) ->
      EnTechInputMap

    # make a complete mapping to be able to look up with sector + subsector + tech the
    # input name to use for an input-driver. Filter for industrial sector as all others are
    # accounted for in previous EnTechInputMap
    bind_rows(
      get_data(all_data, "energy/calibrated_techs") %>% select(supplysector, subsector, technology, minicam.energy.input),
      get_data(all_data, "gcam-china/calibrated_techs_bld_china") %>% select(supplysector, subsector, technology, minicam.energy.input),
      get_data(all_data, UCD_tech_map_name) %>% select(supplysector, subsector = tranSubsector, technology = tranTechnology, minicam.energy.input)
    ) %>%
      rename(stub.technology = technology,
             input.name = minicam.energy.input) %>%
      distinct() ->
      EnTechInputNameMap

    # FIXED: Expand EnTechInputNameMap to include new building sector names
    EnTechInputNameMap_from_bld <- L244.StubTechCalInput_bld_gcamchina %>%
      select(supplysector, subsector, stub.technology) %>%
      distinct() %>%
      mutate(input.name = case_when(
        subsector == "electricity" ~ "electricity",
        subsector == "gas" ~ "delivered gas",
        subsector == "refined liquids" ~ "refined liquids enduse",
        subsector == "coal" ~ "delivered coal",
        subsector == "biomass" ~ "delivered biomass",
        subsector == "trad biomass" ~ "traditional biomass",
        subsector == "traditional biomass" ~ "traditional biomass",
        subsector == "district heat" ~ "district heat",
        subsector == "heat" ~ "district heat",
        TRUE ~ subsector
      ))

    EnTechInputNameMap <- bind_rows(
      EnTechInputNameMap,  # ← Use the variable that was actually created
      EnTechInputNameMap_from_bld
    ) %>%
      distinct()

    # ===================================================

    # 2. Build tables for CSVs
    # Refining emissions
    L241.nonco2_tech_coeff %>%
      filter(region == gcamchina.REGION & Non.CO2 %in% emissions.GHG_NAMES & supplysector == "refining") ->
      L241.ref_ghg_tech_coeff_CHINA

    L222.StubTech_en_CHINA%>%
      repeat_add_columns(tibble("year" = unique(L241.ref_ghg_tech_coeff_CHINA$year))) %>%
      repeat_add_columns(tibble("Non.CO2" = unique(L241.ref_ghg_tech_coeff_CHINA$Non.CO2))) %>%
      left_join(L241.ref_ghg_tech_coeff_CHINA %>%
                  select("subsector", "stub.technology", "year", "Non.CO2", "emiss.coeff"),
                by = c("subsector", "stub.technology", "year", "Non.CO2")) %>%
      na.omit %>%
      select("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "emiss.coeff") ->
      L273.ref_ghg_tech_coeff_CHINA

    L273.ref_ghg_tech_coeff_CHINA %>%
      mutate(emiss.coeff = round(emiss.coeff, emissions.DIGITS_EMISSIONS)) %>%
      arrange(region, supplysector, subsector, stub.technology, year, Non.CO2) %>%
      left_join_error_no_match(EnTechInputMap %>% select(-supplysector), by = c("subsector", "stub.technology"))->
      L273.en_ghg_tech_coeff_CHINA

    # 2c. Input Emissions
    L201.en_ghg_emissions %>%
      filter(region == gcamchina.REGION & !grepl("trn",supplysector)) %>%
      spread(Non.CO2, input.emissions) %>%
      mutate(supplysector = if_else(grepl("comm",supplysector) & subsector == "coal","comm heating", supplysector)) ->
      en_ghg_emissions_CHINA

    # Fertilizer
    L1322.in_EJ_province_Fert_Yh %>%
      mutate(technology = fuel) %>%
      filter(year %in% en_ghg_emissions_CHINA$year) ->
      fert_fuel_input_province

    # Industry
    L232.StubTechCalInput_indenergy_CHINA%>%
      filter(year %in% en_ghg_emissions_CHINA$year) %>%
      left_join_keep_first_only(en_ghg_emissions_CHINA %>%
                                  select("supplysector", "subsector", "stub.technology") %>%
                                  mutate(keep = TRUE),
                                by = c("supplysector","subsector","stub.technology")) %>%
      filter(keep) %>%
      select(region, supplysector, subsector, stub.technology, year, calibrated.value) %>%
      rename(fuel_input = calibrated.value, sector = supplysector, fuel = subsector,
             technology = stub.technology, province = region) ->
      ind_fuel_input_province

    ind_fuel_input_province %>%
      rename(value = fuel_input) %>%
      bind_rows(fert_fuel_input_province) ->
      fuel_input_province

    fuel_input_province %>%
      group_by(sector, fuel, technology, year) %>%
      summarise(fuel_input = sum(value)) ->
      fuel_input_CHINA

    fuel_input_province %>%
      left_join_error_no_match(fuel_input_CHINA, by = c("sector", "fuel", "technology", "year")) %>%
      mutate(fuel_input_share = value / fuel_input) %>%
      left_join(en_ghg_emissions_CHINA %>%
                  select("supplysector", "subsector", "stub.technology",
                         "year", "N2O", "CH4"),
                by = c("sector" = "supplysector",
                       "fuel" = "subsector",
                       "technology" = "stub.technology",
                       "year")) %>%
      mutate(CH4 = fuel_input_share * CH4,
             N2O = fuel_input_share * N2O) %>%
      rename(region = province, supplysector = sector, subsector = fuel, stub.technology = technology) %>%
      select(region, supplysector, subsector, stub.technology, year, CH4, N2O) ->
      en_ghg_emissions_province

    # ============================================================================
    # FINAL FIXED: Buildings GHG Emissions Section
    # ============================================================================
    #
    # COMPLETE MAPPING ANALYSIS:
    #
    # L244.StubTechCalInput_bld_gcamchina structure:
    #   supplysector examples:
    #     - resid_urban heating modern_d1 ~ _d10
    #     - resid_rural heating modern_d1 ~ _d10
    #     - resid_urban cooling modern_d1 ~ _d10
    #     - resid_urban appliances modern_d1 ~ _d10  (subsector: electricity only!)
    #     - resid_urban lighting modern_d1 ~ _d10    (subsector: electricity only!)
    #     - resid_urban hot water_cooking modern_d1  (subsector: coal, electricity, gas, refined liquids)
    #     - comm heating, comm cooling, comm appliances, comm lighting, comm hot water_cooking
    #
    # L201.en_ghg_emissions_CHINA structure:
    #   supplysector examples:
    #     - resid heating modern_d1 ~ _d10    (subsector: biomass, gas, refined liquids)
    #     - resid heating coal_d1 ~ _d10      (subsector: coal)
    #     - resid heating TradBio_d1 ~ _d10   (subsector: traditional biomass)
    #     - resid cooling modern_d1 ~ _d10    (subsector: gas)
    #     - resid others modern_d1 ~ _d10     (subsector: biomass, gas, refined liquids)
    #     - resid others coal_d1 ~ _d10       (subsector: coal)
    #     - resid others TradBio_d1 ~ _d10    (subsector: traditional biomass)
    #     - comm heating                       (subsector: biomass, coal, gas, refined liquids)
    #     - comm cooling                       (subsector: gas)
    #     - comm others                        (subsector: biomass, coal, gas, refined liquids)
    #
    # KEY INSIGHTS:
    # 1. electricity and heat subsectors have NO emissions in L201 (correct - no direct combustion)
    # 2. L201 splits residential by fuel type: modern, coal, TradBio
    # 3. L244 only has "modern" services, coal/TradBio are separate subsectors
    # 4. appliances and lighting only use electricity -> will correctly get 0 emissions
    # 5. hot water_cooking uses coal, gas, refined liquids -> should match L201's "others" categories
    #
    # MAPPING STRATEGY:
    # For each L244 row, we need to determine:
    #   1. emissions_sector: which L201 supplysector to use
    #   2. Based on the subsector (fuel type), map to the correct L201 variant
    # ============================================================================

    # Function to map NEW supplysector + subsector to OLD emissions sector
    map_to_emissions_sector <- function(supplysector, subsector) {

      # Extract income group suffix if present (_d1, _d2, ..., _d10)
      income_group <- stringr::str_extract(supplysector, "_d[0-9]+$")
      income_group <- ifelse(is.na(income_group), "", income_group)

      # Remove income group suffix for analysis
      base_sector <- gsub("_d[0-9]+$", "", supplysector)

      # Determine the service type and fuel category for L201 mapping
      emissions_sector <- case_when(
        # ===== RESIDENTIAL HEATING =====
        # Different fuel types map to different L201 sectors
        grepl("^resid_urban heating modern|^resid_rural heating modern", base_sector) &
          subsector == "coal" ~ paste0("resid heating coal", income_group),
        grepl("^resid_urban heating modern|^resid_rural heating modern", base_sector) &
          subsector == "traditional biomass" ~ paste0("resid heating TradBio", income_group),
        grepl("^resid_urban heating modern|^resid_rural heating modern", base_sector) &
          subsector %in% c("biomass", "gas", "refined liquids") ~ paste0("resid heating modern", income_group),
        # electricity and heat have no emissions
        grepl("^resid_urban heating modern|^resid_rural heating modern", base_sector) &
          subsector %in% c("electricity", "heat") ~ paste0("resid heating modern", income_group),

        # ===== RESIDENTIAL COOLING =====
        grepl("^resid_urban cooling modern|^resid_rural cooling modern", base_sector) ~
          paste0("resid cooling modern", income_group),

        # ===== RESIDENTIAL GENERIC SERVICES (appliances, lighting, hot water_cooking) =====
        # These all map to "resid others" in L201
        # Different fuel types map to different L201 sectors
        (grepl("^resid_urban appliances|^resid_rural appliances", base_sector) |
           grepl("^resid_urban lighting|^resid_rural lighting", base_sector) |
           grepl("^resid_urban hot water_cooking|^resid_rural hot water_cooking", base_sector)) &
          subsector == "coal" ~ paste0("resid others coal", income_group),

        (grepl("^resid_urban appliances|^resid_rural appliances", base_sector) |
           grepl("^resid_urban lighting|^resid_rural lighting", base_sector) |
           grepl("^resid_urban hot water_cooking|^resid_rural hot water_cooking", base_sector)) &
          subsector == "traditional biomass" ~ paste0("resid others TradBio", income_group),

        (grepl("^resid_urban appliances|^resid_rural appliances", base_sector) |
           grepl("^resid_urban lighting|^resid_rural lighting", base_sector) |
           grepl("^resid_urban hot water_cooking|^resid_rural hot water_cooking", base_sector)) &
          subsector %in% c("biomass", "gas", "refined liquids", "electricity") ~
          paste0("resid others modern", income_group),

        # ===== COMMERCIAL SECTORS =====
        grepl("^comm heating", base_sector) ~ "comm heating",
        grepl("^comm cooling", base_sector) ~ "comm cooling",
        # All other commercial services -> comm others
        grepl("^comm appliances|^comm lighting|^comm hot water_cooking", base_sector) ~ "comm others",

        # Default - keep as is
        TRUE ~ supplysector
      )

      return(emissions_sector)
    }

    # ============================================================================
    # Process Building GHG Emissions
    # ============================================================================

    # Step 1: Prepare building fuel input data and map to emissions sectors
    L244.StubTechCalInput_bld_gcamchina %>%
      filter(year %in% en_ghg_emissions_CHINA$year) %>%
      mutate(emissions_sector = mapply(map_to_emissions_sector,
                                       supplysector,
                                       subsector,
                                       USE.NAMES = FALSE)) %>%
      select(region, emissions_sector, supplysector, subsector,
             stub.technology, year, calibrated.value) ->
      bld_fuel_input_province


    # Step 2: Create aggregate table for total national fuel inputs by emissions category
    bld_fuel_input_province %>%
      group_by(emissions_sector, subsector, year) %>%
      summarise(calibrated.value_total = sum(calibrated.value), .groups = "drop") ->
      bld_fuel_input_agg

    # Step 3: Compute shares and allocate emissions
    bld_fuel_input_province %>%
      left_join(bld_fuel_input_agg,
                by = c("emissions_sector", "subsector", "year")) %>%
      # Calculate share, handling zero/zero cases
      mutate(share = if_else(calibrated.value_total > 0,
                             calibrated.value / calibrated.value_total,
                             0)) %>%
      replace_na(list(share = 0)) %>%
      # Join with emissions data using mapped sector names AND subsector
      left_join(en_ghg_emissions_CHINA %>%
                  filter(grepl("resid|comm", supplysector)) %>%
                  select("supplysector", "subsector", "year", "CH4", "N2O"),
                by = c("emissions_sector" = "supplysector",
                       "subsector" = "subsector",
                       "year")) %>%
      # Replace NA emissions with 0 (expected for electricity, heat)
      replace_na(list(CH4 = 0, N2O = 0)) %>%
      # Calculate province-level emissions based on share
      mutate(CH4 = share * CH4,
             N2O = share * N2O) %>%
      # Keep the NEW supplysector names
      select(region, supplysector, subsector, stub.technology, year, CH4, N2O) ->
      bld_ghg_emissions_province


    # ============================================================================
    # END OF FIXED BUILDING EMISSIONS SECTION
    # ============================================================================

    # Combine the buildings and other energy input emissions tables
    en_ghg_emissions_province %>%
      bind_rows(bld_ghg_emissions_province) %>%
      gather(Non.CO2, input.emissions, -region, -supplysector, -subsector, -stub.technology, -year, convert=TRUE) %>%
      arrange(region, supplysector, subsector, stub.technology, Non.CO2, year) ->
      L273.en_ghg_emissions_CHINA

    # Format for csv file
    L273.en_ghg_emissions_CHINA %>%
      select(LEVEL2_DATA_NAMES$StubTechYr, "Non.CO2", "input.emissions") %>%
      mutate(input.emissions = round(input.emissions, emissions.DIGITS_EMISSIONS)) %>%
      # Filter heat related subsector in electricity as they have no direct emissions WDJDec,2026
      filter(!subsector %in% c("electricity", "heat", "district heat")) %>%
      left_join_keep_first_only(EnTechInputNameMap %>% select(-stub.technology), by = c("supplysector", "subsector")) %>%
      na.omit() ->
      L273.en_ghg_emissions_CHINA

    # 2d. Output emissions - HFC/PFC
    L241.hfc_all %>%
      bind_rows(L241.pfc_all) %>%
      filter(region == gcamchina.REGION) ->
      L241.hfc_pfc_CHINA

    L241.hfc_pfc_elec_ownuse <- filter(L241.hfc_pfc_CHINA, supplysector == "electricity_net_ownuse")
    L241.hfc_pfc_bld <- filter(L241.hfc_pfc_CHINA, grepl("cooling", supplysector))

    L123.out_EJ_province_ownuse_elec %>%
      filter(year %in% L241.hfc_pfc_CHINA$year) %>%
      mutate(supplysector = "electricity_net_ownuse",
             subsector = supplysector,
             stub.technology = supplysector) ->
      L123.out_EJ_province_ownuse_elec.long

    L123.out_EJ_province_ownuse_elec.long %>%
      group_by(sector, fuel, year) %>%
      summarise(value = sum(value)) %>%
      ungroup ->
      L123.out_EJ_ownuse_elec_agg

    L123.out_EJ_province_ownuse_elec.long %>%
      left_join_error_no_match(L123.out_EJ_ownuse_elec_agg %>%
                                 select(value2 = value, year),
                               by = "year") %>%
      mutate(share = value / value2) %>%
      repeat_add_columns(tibble("Non.CO2" = unique(L241.hfc_pfc_elec_ownuse$Non.CO2))) %>%
      left_join(L241.hfc_pfc_elec_ownuse %>%
                  select(year, Non.CO2, input.emissions),
                by = c("year", "Non.CO2")) %>%
      mutate(output.emissions = share * input.emissions) %>%
      select(province, supplysector, subsector, stub.technology, year, Non.CO2, output.emissions) %>%
      left_join_error_no_match(province_subregions, by = "province") %>%
      group_by(grid.region, supplysector, subsector, stub.technology, year, Non.CO2) %>%
      summarise(output.emissions = sum(output.emissions)) %>%
      ungroup() %>%
      rename(region = grid.region) ->
      L273.out_ghg_emissions_elec_ownuse

    # HFC/PFC for cooling - map new sectors to old
    map_new_cooling_to_old_hfc <- function(supplysector) {
      income_group <- stringr::str_extract(supplysector, "_d[0-9]+$")
      income_group <- ifelse(is.na(income_group), "", income_group)
      base_sector <- gsub("_d[0-9]+$", "", supplysector)

      case_when(
        grepl("^resid_urban cooling", base_sector) ~ paste0("resid cooling modern", income_group),
        grepl("^resid_rural cooling", base_sector) ~ paste0("resid cooling modern", income_group),
        grepl("^comm cooling", base_sector) ~ "comm cooling",
        TRUE ~ supplysector
      )
    }

    L244.StubTechCalInput_bld_gcamchina %>%
      filter(grepl("cooling", supplysector)) %>%
      mutate(cooling_sector_old = map_new_cooling_to_old_hfc(supplysector)) %>%
      inner_join(L241.hfc_pfc_bld %>%
                   select("supplysector","subsector","year") %>%
                   distinct(),
                 by = c("cooling_sector_old" = "supplysector", "subsector", "year")) %>%
      left_join_error_no_match(L244.GlobalTechEff_bld_gcamchina, by = c("supplysector" = "sector.name",
                                                                   "subsector" = "subsector.name",
                                                                   "stub.technology" = "technology",
                                                                   "year")) %>%
      mutate(service_output = calibrated.value * efficiency) %>%
      select(region, supplysector, subsector, stub.technology, year, service_output, cooling_sector_old) ->
      L244.output_bld_cool

    L244.output_bld_cool %>%
      group_by(cooling_sector_old, subsector, year) %>%
      summarise(service_output = sum(service_output), .groups = "drop") ->
      L244.output_bld_cool_agg

    L244.output_bld_cool %>%
      left_join(L244.output_bld_cool_agg %>%
                  select(cooling_sector_old, subsector, year, service_output2 = service_output),
                by = c("cooling_sector_old", "subsector", "year")) %>%
      mutate(share = if_else(service_output2 > 0, service_output / service_output2, 0)) %>%
      repeat_add_columns(tibble("Non.CO2" = unique(L241.hfc_pfc_bld$Non.CO2))) %>%
      left_join(L241.hfc_pfc_bld %>%
                  select("supplysector", "subsector", "year", "Non.CO2", "input.emissions"),
                by = c("cooling_sector_old" = "supplysector", "subsector", "year", "Non.CO2")) %>%
      mutate(output.emissions = share * input.emissions) %>%
      select("region", "supplysector", "subsector", "stub.technology", "year", "Non.CO2", "output.emissions") ->
      L273.out_ghg_emissions_bld_cool

    bind_rows(L273.out_ghg_emissions_elec_ownuse,
              L273.out_ghg_emissions_bld_cool) %>%
      filter(!grepl("HFC134a",Non.CO2)) ->
      L273.out_ghg_emissions_CHINA

    # MAC curves
    L252.MAC_higwp_CHINA <- filter(L252.MAC_higwp, region == gcamchina.REGION)

    new_cooling_techs <- L273.out_ghg_emissions_bld_cool %>%
      select(supplysector, stub.technology) %>%
      distinct() %>%
      mutate(old_sector = map_new_cooling_to_old_hfc(supplysector))

    L252.MAC_higwp_CHINA %>%
      filter(grepl("cooling", supplysector)) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control,
             tax, mac.reduction, market.name) %>%
      inner_join(new_cooling_techs,
                 by = c("supplysector" = "old_sector"),
                 relationship = "many-to-many") %>%
      mutate(
        supplysector = supplysector.y,
        stub.technology = stub.technology.y
      ) %>%
      select(-supplysector.y, -stub.technology.y, -stub.technology.x) %>%
      select(-region) %>%
      repeat_add_columns(tibble("region" = province_subregions$province)) %>%
      filter(!grepl("HFC134a", Non.CO2)) ->
      L273.MAC_higwp_bld_cool

    L252.MAC_higwp_CHINA %>%
      filter(supplysector %in% L273.out_ghg_emissions_elec_ownuse$subsector) %>%
      select(-region) %>%
      repeat_add_columns(tibble("region" = province_subregions$grid.region)) ->
      L273.MAC_higwp_elec_ownuse

    L273.MAC_higwp_bld_cool %>%
      bind_rows(L273.MAC_higwp_elec_ownuse) %>%
      select(any_of(names(L252.MAC_higwp))) ->
      L273.MAC_higwp_CHINA

    L252.MAC_higwp_tc_average <- filter(L252.MAC_higwp_tc_average, region == gcamchina.REGION) %>%
      select(mac.control, tech.change.year, tech.change) %>%
      distinct()

    L273.MAC_higwp_TC_CHINA <- L273.MAC_higwp_CHINA %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control) %>%
      distinct() %>%
      left_join(L252.MAC_higwp_tc_average, by = "mac.control")

    L252.MAC_higwp_phaseInTime <- filter(L252.MAC_higwp_phaseInTime, region == gcamchina.REGION) %>%
      select(mac.control, mac.phase.in.time) %>%
      distinct()

    L273.MAC_higwp_phaseInTime_CHINA <- L273.MAC_higwp_CHINA %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, mac.control) %>%
      distinct() %>%
      left_join(L252.MAC_higwp_phaseInTime, by = "mac.control")

    # ===================================================
    # Produce outputs

    L273.en_ghg_tech_coeff_CHINA %>%
      add_title("GHG emissions coefficients for energy technologies in China provinces") %>%
      add_units("NA") %>%
      add_comments("Write the China coefficients for every province.") %>%
      add_legacy_name("L273.en_ghg_tech_coeff_CHINA") %>%
      # add_precursors("gcam-china/provinces_subregions",
      add_precursors("gcam-china/province_names_mappings",
                     "L123.out_EJ_province_ownuse_elec",
                     "L1322.in_EJ_province_Fert_Yh",
                     "L201.en_ghg_emissions",
                     "L241.nonco2_tech_coeff",
                     "L241.hfc_all",
                     "L241.pfc_all",
                     "L252.MAC_higwp",
                     "energy/A22.globaltech_input_driver",
                     "energy/A23.globaltech_input_driver",
                     "energy/A25.globaltech_input_driver",
                     "L222.StubTech_en_CHINA",
                     "L232.StubTechCalInput_indenergy_CHINA",
                     "L244.StubTechCalInput_bld_gcamchina",
                     "L244.GlobalTechEff_bld_gcamchina") ->
      L273.en_ghg_tech_coeff_CHINA

    L273.en_ghg_emissions_CHINA %>%
      add_title("Calibrated input emissions of N2O and CH4 by China province") %>%
      add_units("Tg") %>%
      add_comments("Compute shares of national and sector total for each fuel input technology") %>%
      add_legacy_name("L273.en_ghg_emissions_CHINA") %>%
      # add_precursors("province_subregions",
      add_precursors("gcam-china/province_names_mappings",
                     "L123.out_EJ_province_ownuse_elec",
                     "L1322.in_EJ_province_Fert_Yh",
                     "L201.en_ghg_emissions",
                     "energy/calibrated_techs",
                     "gcam-china/calibrated_techs_bld_china",
                     "energy/mappings/UCD_techs_revised",
                     "L241.nonco2_tech_coeff",
                     "L241.hfc_all",
                     "L241.pfc_all",
                     "L252.MAC_higwp",
                     "L222.StubTech_en_CHINA",
                     "L232.StubTechCalInput_indenergy_CHINA",
                     "L244.StubTechCalInput_bld_gcamchina",
                     "L244.GlobalTechEff_bld_gcamchina") ->
      L273.en_ghg_emissions_CHINA

    L273.out_ghg_emissions_CHINA %>%
      add_title("Output emissions of GHGs in China provinces") %>%
      add_units("Gg") %>%
      add_comments("Use province energy data to determine each province's share of the national emissions.") %>%
      add_legacy_name("L273.out_ghg_emissions_CHINA") %>%
      # add_precursors("province_subregions",
      add_precursors("gcam-china/province_names_mappings",
                     "L123.out_EJ_province_ownuse_elec",
                     "L1322.in_EJ_province_Fert_Yh",
                     "L201.en_ghg_emissions",
                     "L241.nonco2_tech_coeff",
                     "L241.hfc_all",
                     "L241.pfc_all",
                     "L252.MAC_higwp",
                     "L222.StubTech_en_CHINA",
                     "L232.StubTechCalInput_indenergy_CHINA",
                     "L244.StubTechCalInput_bld_gcamchina",
                     "L244.GlobalTechEff_bld_gcamchina") ->
      L273.out_ghg_emissions_CHINA

    L273.MAC_higwp_CHINA %>%
      add_title("Abatement curves for the HFCs and PFCs in all China provinces") %>%
      add_units("tax: 1990 USD; mac.reduction: % reduction") %>%
      add_comments("The MAC curves will be identical to those for China.") %>%
      add_legacy_name("L252.MAC_higwp") %>%
      # add_precursors("province_subregions",
      add_precursors("gcam-china/province_names_mappings",
                     "L123.out_EJ_province_ownuse_elec",
                     "L1322.in_EJ_province_Fert_Yh",
                     "L201.en_ghg_emissions",
                     "L241.nonco2_tech_coeff",
                     "L241.hfc_all",
                     "L241.pfc_all",
                     "L252.MAC_higwp",
                     "L222.StubTech_en_CHINA",
                     "L232.StubTechCalInput_indenergy_CHINA",
                     "L244.StubTechCalInput_bld_gcamchina",
                     "L244.GlobalTechEff_bld_gcamchina") ->
      L273.MAC_higwp_CHINA

    L273.MAC_higwp_TC_CHINA %>%
      add_title("Technological change for the MACs of HFCs and PFCs in all China provinces") %>%
      add_units("% improvement of maximum reduction potential per year") %>%
      add_comments("This will be identical to those for China.") %>%
      add_legacy_name("L273.MAC_higwp_TC_CHINA") %>%
      same_precursors_as("L273.MAC_higwp_CHINA") %>%
      add_precursors("L252.MAC_higwp_tc_average") ->
      L273.MAC_higwp_TC_CHINA

    L273.MAC_higwp_phaseInTime_CHINA %>%
      add_title("phase in time for the MACs of HFCs and PFCs in all China provinces") %>%
      add_units("years of maximum reduction potential to be fully phased in") %>%
      add_comments("This will be identical to those for China.") %>%
      add_legacy_name("L273.MAC_higwp_phaseInTime_CHINA") %>%
      same_precursors_as("L273.MAC_higwp_CHINA") %>%
      add_precursors("L252.MAC_higwp_phaseInTime") ->
      L273.MAC_higwp_phaseInTime_CHINA


    L273.en_ghg_tech_coeff_CHINA <- rename(L273.en_ghg_tech_coeff_CHINA, emiss.coef = emiss.coeff)
    L273.out_ghg_emissions_CHINA <- rename(L273.out_ghg_emissions_CHINA, input.emissions = output.emissions)

   return_data(L273.en_ghg_tech_coeff_CHINA, L273.en_ghg_emissions_CHINA, L273.out_ghg_emissions_CHINA,
                L273.MAC_higwp_CHINA, L273.MAC_higwp_TC_CHINA, L273.MAC_higwp_phaseInTime_CHINA)

  } else {
    stop("Unknown command")
  }
}
