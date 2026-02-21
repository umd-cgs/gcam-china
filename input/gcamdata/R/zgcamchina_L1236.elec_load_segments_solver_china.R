# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L1236.elec_load_segments_solver
#'
#' Calculate the fraction of electricity generation by fuel by horizontal load segment such that the total supply
#' of electricity in each grid region matches total demand of electricity in that grid region.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1236.grid_elec_supply_USA}.
#'
#' The corresponding file in the original data system was \code{LB1236.elec_load_segments_solver_2010.R} (gcam-usa level1).
#' @details Calculates the fraction of electricity generation by fuel, by horizontal load segment, by grid region, in 2010.
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter mutate pull select
#' @author MTB August 2018 / Kanishka Narayan July 2023 / YangOu Dec 2023
module_gcamchina_L1236.elec_load_segments_solver <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/elecS_horizontal_to_vertical_map",
                    "L1234.out_EJ_grid_elec_F_CHINA",
                    "L1235.grid_elec_supply_CHINA",
                    "L1235.elecS_demand_fraction_CHINA",
                    "L1235.elecS_horizontal_vertical_CHINA",
                    "L1235.elecS_horizontal_vertical_GCAM_coeff_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1236.grid_elec_supply_CHINA",
             "L1236.elecS_demand_fraction_adj_CHINA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    year <- fuel <- grid_region <- sector <- tot_generation <- generation <- fraction <- horizontal_segment <-
      vertical_segment <- data <- elec_fuel <- load_segment <- new_fraction <- segment <- non_segment_frac <-
      generation.x <- vertical_segment_demand <- horizontal_segment_demand  <- off.peak.electricity <-
      base_intermediate  <- intermediate.electricity <- base_subpeak <- subpeak.electricity <- base_peak  <-
      peak.electricity <- int_subpeak <- subpeak_peak <- check <- pct_check <- grid_total <- grid_share_fuel <-
      root <- NULL # silence package check notes

    # Load required inputs
    elecS_horizontal_to_vertical_map <- get_data(all_data, "gcam-china/elecS_horizontal_to_vertical_map")
    L1234.out_EJ_grid_elec_F <- get_data(all_data, "L1234.out_EJ_grid_elec_F_CHINA") %>%
      rename(grid_region = grid.region)
    L1235.grid_elec_supply_CHINA <- get_data(all_data, "L1235.grid_elec_supply_CHINA", strip_attributes = TRUE)
    L1235.elecS_demand_fraction_CHINA <- get_data(all_data, "L1235.elecS_demand_fraction_CHINA", strip_attributes = TRUE)
    L1235.elecS_horizontal_vertical_CHINA <- get_data(all_data, "L1235.elecS_horizontal_vertical_CHINA", strip_attributes = TRUE)
    L1235.elecS_horizontal_vertical_GCAM_coeff_CHINA <- get_data(all_data, "L1235.elecS_horizontal_vertical_GCAM_coeff_CHINA", strip_attributes = TRUE)


    # ===================================================
    # Data Processing

    # Initialize Variables
    L1236.elecS_demand_fraction <- L1235.elecS_demand_fraction_CHINA
    L1236.elecS_horizontal_vertical <- L1235.elecS_horizontal_vertical_CHINA

    # Filter for years for which electricity load segments will be calibrated
    L1236.grid_elec_supply <- L1235.grid_elec_supply_CHINA %>%
      filter(year %in% gcamchina.LOAD_SEG_CAL_YEARS)

    # Summarize generation by year / grid region / fuel; remove distinction between solar PV & CSP
    L1234.out_EJ_grid_elec_F %>%
      filter(year %in% gcamchina.LOAD_SEG_CAL_YEARS) %>%
      mutate(fuel = sub("solar CSP", "solar", fuel),
             fuel = sub("solar PV", "solar", fuel)) %>%
      group_by(grid_region, sector, year, fuel) %>%
      summarise(tot_generation = sum(generation), .groups = "drop") %>%
      ungroup() -> L1236.out_EJ_grid_elec_F

    # Join in total generation data by year / grid region / fuel from L1236.out_EJ_grid_elec_F
    L1236.grid_elec_supply %>%
      left_join_error_no_match(L1236.out_EJ_grid_elec_F,
                               by = c("grid_region", "year", "fuel")) %>%
      select(grid_region, segment, fuel, year, tot_generation, fraction, generation ) -> L1236.grid_elec_supply

    # Create a table to hold data for years for which electricity load segments will not be calibrated
    # Even though the electricity load segment shares for these years will not be calibrated,
    # we need to carry this historical data forward.
    # Process is similar to the above for calibrated years:
    # Remove distinction between solar PV & CSP;
    # Summarize generation by year / grid region / fuel;
    # Filter for years for which electricity load segments will not be calibrated
    L1235.grid_elec_supply_CHINA %>%
      filter(!(year %in% gcamchina.LOAD_SEG_CAL_YEARS)) %>%
      left_join_error_no_match(L1234.out_EJ_grid_elec_F %>%
                                 mutate(fuel = sub("solar CSP", "solar", fuel),
                                        fuel = sub("solar PV", "solar", fuel)) %>%
                                 group_by(grid_region, sector, year, fuel) %>%
                                 summarise(tot_generation = sum(generation), .groups = "drop") %>%
                                 ungroup(),
                               by = c("grid_region", "year", "fuel")) %>%
      select(grid_region, segment, fuel, year, tot_generation, fraction, generation ) -> L1236.grid_elec_supply_non_cal

    # List of horizontal and vertical electricity segments
    L1236.segment_list <- unique(elecS_horizontal_to_vertical_map$horizontal_segment)
    L1236.vertical_segment_list <- unique(elecS_horizontal_to_vertical_map$vertical_segment)

    L1236.gridregion_list <- unique(L1236.grid_elec_supply$grid_region)

    # Function for replacing the existing fraction of a fuel consumed in a given load segment
    # (by grid region & year) with a new value
    replace_fraction <- function(data, elec_fuel, load_segment, new_fraction) {
      data %>%
        mutate(fraction = replace(fraction, grid_region == L1236.region &
                                    fuel == elec_fuel &
                                    segment == load_segment &
                                    year == segment_year,
                                  new_fraction))
    }

    # Function for calculating the fraction of a fuel consumed by other load segments in a given grid region & year
    calc_non_segment_frac <- function(data, elec_fuel, load_segment) {
      data %>%
        filter(grid_region == L1236.region &
                 fuel == elec_fuel &
                 segment != load_segment &
                 year == segment_year) %>%
        summarise(non_segment_frac = sum(fraction), .groups = "drop") %>%
        pull(non_segment_frac)
    }

    # Function to check that electricity demands and supplies match by load segment and grid region
    # Function is subsequently solved by uniroot() - One Dimensional Root (Zero) Finding -
    # see https://www.rdocumentation.org/packages/stats/versions/3.5.3/topics/uniroots
    check_elec_segments <- function(gen_fraction, L1236.region, L1236.segment, L1236.fuel = "gas") {

      # Set fraction as specified
      L1236.grid_elec_supply %>%
        replace_fraction(L1236.fuel, L1236.segment, gen_fraction) -> L1236.grid_elec_supply

      # If fuel == gas or oil, adjust fraction of fuel consumed in peak load segment to make sure that sum of fractions is 1
      if (L1236.fuel == "gas" | L1236.fuel == "refined liquids") {

        L1236.grid_elec_supply %>%
          calc_non_segment_frac(L1236.fuel, gcamchina.ELEC_SEGMENT_PEAK) -> L1236.non_peak

        L1236.grid_elec_supply %>%
          replace_fraction(L1236.fuel, gcamchina.ELEC_SEGMENT_PEAK, 1 - L1236.non_peak) -> L1236.grid_elec_supply

      }


      # If fuel == coal, adjust fraction of fuel consumed in base load segment to make sure sum of fractions is 1
      if (L1236.fuel == "hydro" | L1236.fuel == "coal") {

        L1236.grid_elec_supply %>%
          calc_non_segment_frac(L1236.fuel, gcamchina.ELEC_SEGMENT_BASE) -> L1236.non_base

        L1236.grid_elec_supply %>%
          replace_fraction(L1236.fuel, gcamchina.ELEC_SEGMENT_BASE, 1 - L1236.non_base) -> L1236.grid_elec_supply

      }

      L1236.grid_elec_supply %>%
        mutate(generation = tot_generation * fraction) -> L1236.grid_elec_supply

      # Calculate electricity supply by horizontal segment in each grid region by aggregating all technologies
      L1236.grid_elec_supply %>%
        group_by(grid_region, segment, year) %>%
        summarise(generation = sum(generation), .groups = "drop") %>%
        ungroup() -> L1236.grid_check

      # Calculate electricity demand for each horizontal segment in each grid region
      L1236.grid_elec_supply %>%
        group_by(grid_region, year) %>%
        summarise(tot_demand = sum(generation),.groups = "drop") %>%
        ungroup() -> L1236.grid_elec_demand

      L1236.grid_check %>%
        left_join_error_no_match(L1236.grid_elec_demand,
                                 by = c("grid_region","year")) %>%
        left_join_error_no_match(elecS_horizontal_to_vertical_map,
                                 by = c("segment" = "horizontal_segment")) %>%
        left_join_error_no_match (L1236.elecS_demand_fraction ,
                                  by = c("grid_region", "vertical_segment")) %>%
        mutate(vertical_segment_demand = tot_demand * demand_fraction) -> L1236.grid_elec_demand

      L1236.grid_check %>%
        left_join_error_no_match(L1236.grid_elec_demand,
                                 by = c("grid_region", "segment", "year")) %>%
        select(grid_region, segment, year, generation.x, vertical_segment_demand) %>%
        rename(generation = generation.x) -> L1236.grid_check

      # Prepare tables to check that supplies and demands balance for each load segment.  For each horizontal (supply-side) load segment:
      # (1) Filter for the relevant load segment.
      # (2) Join L1236.elecS_horizontal_vertical.  This table outlines how generation in the horizontal (supply-side) load segments -
      # base load generation, intermeidate generation, subpeak generation, peak generation - are shared across the four vertical
      # (demand-side) load segments - off.peak.electricity, intermediate.electricity, subpeak.electricity, peak.electricity.
      # (3) Calculate the size of generation in the horizontal load segment across all of the relevant vertical segments.
      # For example, base load generation provides all of off.peak.electricity demand plus a portion of intermediate.electricity,
      # subpeak.electricity, and peak.electricity demands.  Intermediate generation serves the remaining portion of
      # intermediate.electricity as well as some subpeak.electricity and peak.electricity demands.  Peak generation serves only the
      # portion of peak.electricity demands not met by generation from the other horizontal load segments.

      L1236.grid_check %>%
        filter(segment == gcamchina.ELEC_SEGMENT_BASE ) %>%
        left_join_error_no_match(L1236.elecS_horizontal_vertical,
                                 by = c("grid_region", "segment" = "horizontal_segment")) %>%
        # Calculate total demand for base load generation. This is equal to the demand for off.peak.electricity divided by the
        # share of base load generation that serves off.peak.electricity (to account for the fact that base load generation
        # also serves a portion of intermediate.electricity, subpeak.electricity, and peak.electricity demands).
        mutate(horizontal_segment_demand = vertical_segment_demand / off.peak.electricity,
               # The below three calculations are not relevant for base load generation but will be used in calculations
               # for the other three horizontal load segments below.
               # Calculate amount of base load generation that serves the vertical intermediate.electricity segment
               base_intermediate = horizontal_segment_demand * intermediate.electricity,
               # Calculate amount of base load generation that serves the vertical subpeak.electricity segment
               base_subpeak = horizontal_segment_demand * subpeak.electricity,
               # Calculate amount of base load generation that serves the vertical peak.electricity segment
               base_peak = horizontal_segment_demand * peak.electricity) -> L1236.grid_check_base

      L1236.grid_check %>%
        filter(segment == gcamchina.ELEC_SEGMENT_INT ) %>%
        left_join_error_no_match(L1236.elecS_horizontal_vertical,
                                 by = c("grid_region", "segment" = "horizontal_segment")) %>%
        left_join_error_no_match(L1236.grid_check_base %>%
                                   select(grid_region, year, base_intermediate),
                                 by = c("grid_region", "year")) %>%
        # Calculate total demand for intermediate generation (horizontal segment).  This is equal to the demand
        # for intermediate.electricity (vertical segment) minus the amount of intermediate.electricity served by
        # base load generation, divided by the share of intermediate generation that serves intermediate.electricity
        # (to account for the fact that intermediate generation also serves a portion of subpeak.electricity and peak.electricity demands).
        mutate(horizontal_segment_demand = (vertical_segment_demand - base_intermediate) /
                 intermediate.electricity ,
               # Calculate amount of intermediate generation that serves the vertical subpeak.electricity segment
               int_subpeak = horizontal_segment_demand * subpeak.electricity,
               # Calculate amount of intermediate generation that serves the vertical peak.electricity segment
               int_peak = horizontal_segment_demand * peak.electricity) -> L1236.grid_check_int

      L1236.grid_check %>%
        filter(segment == gcamchina.ELEC_SEGMENT_SUBPEAK ) %>%
        left_join_error_no_match(L1236.elecS_horizontal_vertical,
                                 by = c("grid_region", "segment" = "horizontal_segment")) %>%
        left_join_error_no_match(L1236.grid_check_base %>%
                                   select(grid_region, year, base_subpeak),
                                 by = c("grid_region", "year")) %>%
        left_join_error_no_match(L1236.grid_check_int %>%
                                   select(grid_region, year, int_subpeak),
                                 by = c("grid_region", "year")) %>%
        # Calculate total demand for subpeak generation (horizontal segment).  This is equal to the demand
        # for subpeak.electricity (vertical segment) minus the amount of  subpeak.electricity served by
        # base load generation and intermediate generation, divided by the share of subpeak generation that serves subpeak.electricity
        # (to account for the fact that subpeak generation also serves a portion of peak.electricity demands).
        mutate(horizontal_segment_demand = (vertical_segment_demand - base_subpeak - int_subpeak) /
                 subpeak.electricity,
               # Calculate amount of subpeak generation that serves the vertical peak.electricity segment
               subpeak_peak = horizontal_segment_demand * peak.electricity) -> L1236.grid_check_subpeak

      L1236.grid_check %>%
        filter(segment == gcamchina.ELEC_SEGMENT_PEAK ) %>%
        left_join_error_no_match(L1236.elecS_horizontal_vertical,
                                 by = c("grid_region", "segment" = "horizontal_segment")) %>%
        left_join_error_no_match(L1236.grid_check_base %>%
                                   select(grid_region, year, base_peak),
                                 by = c("grid_region", "year")) %>%
        left_join_error_no_match(L1236.grid_check_int %>%
                                   select(grid_region, year, int_peak),
                                 by = c("grid_region", "year")) %>%
        left_join_error_no_match(L1236.grid_check_subpeak %>%
                                   select(grid_region, year, subpeak_peak),
                                 by = c("grid_region", "year")) %>%
        # Calculate total demand for peak generation (horizontal segment).  This is equal to the demand
        # for peak.electricity (vertical segment) minus the amount of peak.electricity served by
        # base load generation, intermediate generation, and subpeak generation.
        mutate(horizontal_segment_demand = (vertical_segment_demand - base_peak - int_peak - subpeak_peak) /
                 peak.electricity) ->  L1236.grid_check_peak

      # Filter for the information needed going forward.  We needed to carry some additional information
      # previously to build each of the tables below.
      L1236.grid_check_base %>%
        select(grid_region, segment, year, generation,
               vertical_segment_demand, horizontal_segment_demand) -> L1236.grid_check_base

      L1236.grid_check_int %>%
        select(grid_region, segment, year, generation,
               vertical_segment_demand, horizontal_segment_demand) -> L1236.grid_check_int

      L1236.grid_check_subpeak %>%
        select(grid_region, segment, year, generation,
               vertical_segment_demand, horizontal_segment_demand) -> L1236.grid_check_subpeak

      L1236.grid_check_peak %>%
        select(grid_region, segment, year, generation,
               vertical_segment_demand, horizontal_segment_demand) -> L1236.grid_check_peak

      L1236.grid_check_base %>%
        bind_rows(L1236.grid_check_int, L1236.grid_check_subpeak, L1236.grid_check_peak) -> L1236.grid_check

      # Check that supply meets demand for each load segment, i.e. that generation from a given horizontal
      # electricity load segment matches demand for this generation across the four vertical load segments
      L1236.grid_check %>%
        mutate(check = horizontal_segment_demand - generation,
               pct_check = check / generation) -> L1236.grid_check
      #write.csv(L1236.grid_check,"check.csv")
      L1236.grid_check %>%
        filter(grid_region == L1236.region & segment == L1236.segment & year == segment_year) %>%
        select(check) %>%
        pull(check) -> check

      check
    }

    root_finder <- function(func, interval,...) {
        result <- tryCatch(uniroot(func, interval,...), error = function(e) NULL)
        if (!is.null(result))
          return(result$root)
        else
          return(100)
      }

    # Calculate total electricity generation by year / grid region
    L1236.out_EJ_grid_elec_F %>%
      group_by(grid_region, sector, year) %>%
      summarise(grid_total = sum(tot_generation), .groups = "drop") %>%
      ungroup() -> L1236.grid_total

    # Calculate the share of generation from a given fuel across load segment by year / grid region
    L1236.out_EJ_grid_elec_F %>%
      left_join_error_no_match(L1236.grid_total, by = c("grid_region", "sector", "year")) %>%
      mutate(grid_share_fuel = tot_generation / grid_total) -> L1236.out_EJ_grid_elec_F

    # For each grid region and year, calculate the fraction of electricity generation by fuel by horizontal load segment such that
    # electricity supplies and demands balance.
    for (r in seq_along(L1236.gridregion_list)){
      for(y in seq_along(gcamchina.LOAD_SEG_CAL_YEARS)){

        L1236.region <- L1236.gridregion_list[r]
        segment_year <- gcamchina.LOAD_SEG_CAL_YEARS[y]

        # Calculate fractions of electricity generation by fuel for particular fuels
        L1236.out_EJ_grid_elec_F %>%
          filter(grid_region == L1236.region & fuel == "gas" & year == segment_year) %>%
          select(grid_share_fuel) %>%
          pull(grid_share_fuel) -> L1236.gas_frac

        L1236.out_EJ_grid_elec_F %>%
          filter(grid_region == L1236.region & fuel == "refined liquids" & year == segment_year) %>%
          select(grid_share_fuel) %>%
          pull(grid_share_fuel) -> L1236.oil_frac

        L1236.out_EJ_grid_elec_F %>%
          filter(grid_region == L1236.region & fuel == "coal" & year == segment_year) %>%
          select(grid_share_fuel) %>%
          pull(grid_share_fuel) -> L1236.coal_frac

        L1236.out_EJ_grid_elec_F %>%
          filter(grid_region == L1236.region & fuel == "hydro" & year == segment_year) %>%
          select(grid_share_fuel) %>%
          pull(grid_share_fuel) -> L1236.hydro_frac

        #kbn 2019 : Adding wind_frac and solar_frac
        L1236.out_EJ_grid_elec_F %>%
          filter(grid_region == L1236.region & fuel == "wind" & year == segment_year) %>%
          select(grid_share_fuel) %>%
          pull(grid_share_fuel) -> L1236.wind_frac

        L1236.out_EJ_grid_elec_F %>%
          filter(grid_region == L1236.region & fuel == "solar" & year == segment_year) %>%
          select(grid_share_fuel) %>%
          pull(grid_share_fuel) -> L1236.solar_frac

        L1236.out_EJ_grid_elec_F %>%
          filter(grid_region == L1236.region & fuel == "nuclear" & year == segment_year) %>%
          select(grid_share_fuel) %>%
          pull(grid_share_fuel) -> L1236.nuclear_frac

        L1236.out_EJ_grid_elec_F %>%
          filter(grid_region == L1236.region & fuel == "geothermal" & year == segment_year) %>%
          select(grid_share_fuel) %>%
          pull(grid_share_fuel) -> L1236.geothermal_frac

        L1236.out_EJ_grid_elec_F %>%
          filter(grid_region == L1236.region & year == segment_year) %>%
          filter(grid_share_fuel >=0.3) %>%
          dplyr::select(fuel,grid_share_fuel) %>%
          distinct()->dominant_fuels

        L1236.out_EJ_grid_elec_F %>%
          filter(grid_region == L1236.region & year == segment_year) %>%
          filter(grid_share_fuel <0.3) %>%
          dplyr::select(fuel,grid_share_fuel) %>%
          distinct() %>%
          filter(grid_share_fuel>0)->non_dominant_fuels

        # Yang Ou Dec 2023
        # Segment-Specific Tuning in Model Calibration
        # ------------------------------------------------------------------------------------------------------------------------
        # This section involves year-by-year tuning, which is the most challenging part of the process. The goal is to ensure
        # that all "dominant fuels" (typically coal and hydro) are accurately accounted for in each segment.
        #
        # Key Principles:
        # - This is primarily a manual process that depends on expert judgement.
        # - Tuning for each region is guided by error messages (e.g., "Warning: subpeak demand could not solve.").
        #   These indicate the need for manual adjustments.
        # - The code contains numerous "print" statements to identify issues with specific fuel types, years, or regions.
        #   If a fuel cannot be allocated to a segment due to space constraints, manual adjustments are necessary.
        #
        # Common Scenarios:
        # - Refined liquids and gas are usually allocated to subpeak and peak segments. If a peak segment is fully
        #   occupied by refined liquids, leaving no space for coal, reallocate refined liquids to non-peak segments.
        #
        # Note:
        # - This calibration is crucial for the accurate historical performance of the model but does not impact future
        #   periods. The base/int/subpeak/peak segments are used only for model allocation balance, not for real capacity dispatch.
        # - This approach aligns with the GCAM-USA. For more details, see the "Electricity generation" section in:
        #   https://jgcri.github.io/gcam-doc/gcam-usa.html
        # - Users typically do not need to modify this part, as the current version is already extensively adjusted to make sure
        #   there is no warning (i.e. all fuel/segment/grid have been solved)
        # ------------------------------------------------------------------------------------------------------------------------




        # tune each year, no particular order
        # temperatly delete 2021 part
        if (segment_year %in% c(2021)){

          for (i in unique(dominant_fuels$fuel)){

            # 1) East China Grid
            if (L1236.region %in% c("East China Grid")){
              L1236.grid_elec_supply %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_BASE, 0.5) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, 0.45) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.05) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("biomass", gcamusa.ELEC_SEGMENT_BASE, 0.5) %>%
                replace_fraction("biomass", gcamusa.ELEC_SEGMENT_INT, 0.5) %>%
                replace_fraction("biomass", gcamusa.ELEC_SEGMENT_SUBPEAK, 0) %>%
                replace_fraction("biomass", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_BASE, 0.95) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_INT, 0.05) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, 1)-> L1236.grid_elec_supply}


            # 2) Northeast China Grid
            if (L1236.region %in% c("Northeast China Grid")){
              L1236.grid_elec_supply %>%
                replace_fraction("biomass", gcamusa.ELEC_SEGMENT_BASE, 0.1) %>%
                replace_fraction("biomass", gcamusa.ELEC_SEGMENT_INT, 0.9) %>%
                replace_fraction("biomass", gcamusa.ELEC_SEGMENT_SUBPEAK, 0) %>%
                replace_fraction("biomass", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("solar", gcamusa.ELEC_SEGMENT_BASE, 0.) %>%
                replace_fraction("solar", gcamusa.ELEC_SEGMENT_INT, 0.8) %>%
                replace_fraction("solar", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.2) %>%
                replace_fraction("solar", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_BASE, 0.6) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, 0.35) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.05) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_BASE, 0.95) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_INT, 0.05) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, 1)-> L1236.grid_elec_supply
            }

            # 3) Central China Grid
            if (L1236.region %in% c("Central China Grid")){
              L1236.grid_elec_supply %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_BASE, 0.5) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, 0.35) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.15) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_BASE, 0.90) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_INT, 0.10) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, 1)-> L1236.grid_elec_supply
            }

            # 4) China Southern Power Grid
            if (L1236.region %in% c("China Southern Power Grid")){
              L1236.grid_elec_supply %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_BASE, 0.9) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, 0.05) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.05) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 1) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, 0)-> L1236.grid_elec_supply
            }

            # 5) Northwest China Grid
            if (L1236.region %in% c("Northwest China Grid")){
              L1236.grid_elec_supply %>%
                replace_fraction("solar", gcamusa.ELEC_SEGMENT_BASE, 0) %>%
                replace_fraction("solar", gcamusa.ELEC_SEGMENT_INT, 1) %>%
                replace_fraction("solar", gcamusa.ELEC_SEGMENT_SUBPEAK, 0) %>%
                replace_fraction("solar", gcamusa.ELEC_SEGMENT_PEAK, 0) -> L1236.grid_elec_supply}

            # 6) North China Grid
            # ---- North China Grid tuned ----
            if (L1236.region %in% c("North China Grid")) {
              L1236.grid_elec_supply %>%
                replace_fraction('coal', gcamusa.ELEC_SEGMENT_BASE, 0.64) %>%
                replace_fraction('coal', gcamusa.ELEC_SEGMENT_INT, 0.32) %>%
                replace_fraction('coal', gcamusa.ELEC_SEGMENT_SUBPEAK, 0.00) %>%
                replace_fraction('coal', gcamusa.ELEC_SEGMENT_PEAK, 0.04)%>%

                replace_fraction("gas", gcamusa.ELEC_SEGMENT_BASE, 0.3)%>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, 0.4)%>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.3)%>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 0.0)%>%

                replace_fraction("biomass", gcamusa.ELEC_SEGMENT_BASE, 0.8) %>%
                replace_fraction("biomass", gcamusa.ELEC_SEGMENT_INT, 0.2) %>%
                replace_fraction("biomass", gcamusa.ELEC_SEGMENT_SUBPEAK, 0) %>%
                replace_fraction("biomass", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%

                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_BASE, 0.8) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_INT, 0.2) %>%

                replace_fraction("wind", gcamusa.ELEC_SEGMENT_BASE, 0.45) %>%
                replace_fraction("wind", gcamusa.ELEC_SEGMENT_INT, 0.15) %>%
                replace_fraction("wind", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.4) %>%
                replace_fraction("wind", gcamusa.ELEC_SEGMENT_PEAK, 0.00) %>%

                replace_fraction("solar", gcamusa.ELEC_SEGMENT_BASE, 0.00) %>%
                replace_fraction("solar", gcamusa.ELEC_SEGMENT_INT, 0.5) %>%
                replace_fraction("solar", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.3) %>%
                replace_fraction("solar", gcamusa.ELEC_SEGMENT_PEAK, 0.2) %>%

                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.5) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 0.5) -> L1236.grid_elec_supply
            }

            #Solve for int
            L1236.solved_fraction_int <- root_finder(check_elec_segments, c(0, 1), L1236.region, gcamchina.ELEC_SEGMENT_INT, i)
            
            if(L1236.solved_fraction_int>1){
              #print(L1236.region)
              #print(segment_year)
              #print(i)
              #print("Warning intermediate demand could not solve. So, setting it to a pre-determined value")
              L1236.solved_fraction_int$root=0
            }else{
              L1236.solved_fraction_int <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamchina.ELEC_SEGMENT_INT, i)
            }

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamchina.ELEC_SEGMENT_INT, L1236.solved_fraction_int$root) -> L1236.grid_elec_supply

            L1236.grid_elec_supply %>%
              calc_non_segment_frac(i, gcamusa.ELEC_SEGMENT_BASE) -> L1236.non_base

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_BASE, 1 - L1236.non_base) -> L1236.grid_elec_supply

            #Solve for sub-peak
            L1236.solved_fraction_subpeak <- root_finder(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, i)

            if(L1236.solved_fraction_subpeak>1){
              #print(L1236.region)
              #print(segment_year)
              #print(i)
              #print("Warning subpeak demand could not solve. So, setting it to a pre-determined value")
              L1236.solved_fraction_subpeak$root=0
            }else{
              L1236.solved_fraction_subpeak <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, i)
            }

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_SUBPEAK, L1236.solved_fraction_subpeak$root) -> L1236.grid_elec_supply

            L1236.grid_elec_supply %>%
              calc_non_segment_frac(i, gcamchina.ELEC_SEGMENT_BASE) -> L1236.non_base

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamchina.ELEC_SEGMENT_BASE, 1 - L1236.non_base) -> L1236.grid_elec_supply

            #Solve for peak
            L1236.solved_fraction_peak <- root_finder(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_PEAK, i)

            if(L1236.solved_fraction_peak>0.5){
              if (i == "coal"){
                #print(L1236.region)
                #print(segment_year)
                #print(i)
                #print("Warning peak demand could not solve. So, setting it to a pre-determined value")
                L1236.solved_fraction_peak$root=0.01
              } else if (i == "hydro") {
                #print(L1236.region)
                #print(segment_year)
                #print(i)
                #print("Warning peak demand could not solve. So, setting it to a pre-determined value")
                L1236.solved_fraction_peak$root=0
              }
            }else{
              L1236.solved_fraction_peak <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_PEAK, i)
            }
            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_PEAK, L1236.solved_fraction_peak$root) -> L1236.grid_elec_supply

            L1236.grid_elec_supply %>%
              calc_non_segment_frac(i, gcamusa.ELEC_SEGMENT_BASE) -> L1236.non_int

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_BASE, 1 - L1236.non_int) -> L1236.grid_elec_supply

          }

        }


        if (segment_year %in% c(2015)){

          for (i in unique(dominant_fuels$fuel)){

            # region specific adjustment
            # 1) several grids have too much coal and hydro, need to remove all other peak fuels to give them more room in peak
            if (L1236.region %in% c("East China Grid", "Northeast China Grid","Central China Grid")){
              L1236.grid_elec_supply %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_BASE, 0.6) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, 0.25) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.15) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_BASE, 0.95) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_INT, 0.05) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, 1)-> L1236.grid_elec_supply}

            #2) China Southern Power Grid: create space for subpeak
            if (L1236.region %in% c("China Southern Power Grid")){
              L1236.grid_elec_supply %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_BASE, 0.6) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, 0.25) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.15) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 1) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, 0)-> L1236.grid_elec_supply}

            # 2) Northwest has a lot of solar in subpeak, move to intermediate to give more room for coal
            if (L1236.region %in% c("Northwest China Grid")){
              L1236.grid_elec_supply %>%
                replace_fraction("solar", gcamusa.ELEC_SEGMENT_BASE, 0) %>%
                replace_fraction("solar", gcamusa.ELEC_SEGMENT_INT, 1) %>%
                replace_fraction("solar", gcamusa.ELEC_SEGMENT_SUBPEAK, 0) %>%
                replace_fraction("solar", gcamusa.ELEC_SEGMENT_PEAK, 0) -> L1236.grid_elec_supply}

            #Solve for intermediate
            L1236.solved_fraction_int <- root_finder(check_elec_segments, c(0, 1), L1236.region, gcamchina.ELEC_SEGMENT_INT, i)
            
            if(L1236.solved_fraction_int>1){
              #print(L1236.region)
              #print(segment_year)
              #print(i)
              #print("Warning intermediate demand could not solve. So, setting it to a pre-determined value")
              L1236.solved_fraction_int$root=0
            }else{
              L1236.solved_fraction_int <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamchina.ELEC_SEGMENT_INT, i)
            }

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamchina.ELEC_SEGMENT_INT, L1236.solved_fraction_int$root) -> L1236.grid_elec_supply

            L1236.grid_elec_supply %>%
              calc_non_segment_frac(i, gcamusa.ELEC_SEGMENT_BASE) -> L1236.non_base

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_BASE, 1 - L1236.non_base) -> L1236.grid_elec_supply
            
            #Solve for sub-peak
            L1236.solved_fraction_subpeak <- root_finder(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, i)

            if(L1236.solved_fraction_subpeak>1){
              #print(L1236.region)
              #print(segment_year)
              #print(i)
              #print("Warning subpeak demand could not solve. So, setting it to a pre-determined value")
              L1236.solved_fraction_subpeak$root=0
            }else{
              L1236.solved_fraction_subpeak <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, i)
            }

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_SUBPEAK, L1236.solved_fraction_subpeak$root) -> L1236.grid_elec_supply

            L1236.grid_elec_supply %>%
              calc_non_segment_frac(i, gcamchina.ELEC_SEGMENT_BASE) -> L1236.non_base

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamchina.ELEC_SEGMENT_BASE, 1 - L1236.non_base) -> L1236.grid_elec_supply

            #Solve for peak
            L1236.solved_fraction_peak <- root_finder(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_PEAK, i)

            if(L1236.solved_fraction_peak>1){
              #print(L1236.region)
              #print(segment_year)
              #print(i)
              #print("Warning peak demand could not solve. So, setting it to a pre-determined value")
              L1236.solved_fraction_peak$root=0
            }else{
              L1236.solved_fraction_peak <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_PEAK, i)

            }

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_PEAK, L1236.solved_fraction_peak$root) -> L1236.grid_elec_supply

            L1236.grid_elec_supply %>%
              calc_non_segment_frac(i, gcamusa.ELEC_SEGMENT_BASE) -> L1236.non_int

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_BASE, 1 - L1236.non_int) -> L1236.grid_elec_supply

          }

        }

        if (segment_year %in% c(1990)){

          for (i in unique(dominant_fuels$fuel)){

            # region specific adjustment
            # Part 1: relocate more gas and liquids to non-peak loads, so that coal/hydro could go to peak
            if (L1236.region %in% c("Central China Grid", "East China Grid", "North China Grid", "Northeast China Grid",
                                    "Northwest China Grid")){
              L1236.grid_elec_supply %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_BASE, 0.6) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, 0.3) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.1) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_BASE, 0.6) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_INT, 0.3) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.1) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_BASE, 0.8) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_INT, 0.2) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_SUBPEAK, 0) -> L1236.grid_elec_supply}

            # Part 2 : China Southern Power Grid adjustment, allocate more to base/int so that coal can move up to subpeak and peak
            if (L1236.region %in% c("China Southern Power Grid")){
              L1236.grid_elec_supply %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_BASE, 0.5) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_INT, 0.5) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_BASE, 0.7) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, 0.3) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, 0) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_BASE, 0.7) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_INT, 0.3) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, 0) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 0) -> L1236.grid_elec_supply}

            #Solve for int
            L1236.solved_fraction_int <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamchina.ELEC_SEGMENT_INT, i)

            if(L1236.solved_fraction_int$root>1){
              #print(L1236.region)
              #print(segment_year)
              #print(i)
              #print("Warning intermediate demand could not solve. So, setting it to a pre-determined value")
              L1236.solved_fraction_int$root=0
            }else{
              L1236.solved_fraction_int <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamchina.ELEC_SEGMENT_INT, i)

            }

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamchina.ELEC_SEGMENT_INT, L1236.solved_fraction_int$root) -> L1236.grid_elec_supply

            L1236.grid_elec_supply %>%
              calc_non_segment_frac(i, gcamusa.ELEC_SEGMENT_BASE) -> L1236.non_base

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_BASE, 1 - L1236.non_base) -> L1236.grid_elec_supply

            #Solve for sub-peak
            L1236.solved_fraction_subpeak <- root_finder(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, i)

            if(L1236.solved_fraction_subpeak>1){
              #print(L1236.region)
              #print(segment_year)
              #print(i)
              #print("Warning subpeak demand could not solve. So, setting it to a pre-determined value")
              L1236.solved_fraction_subpeak$root=0
            }else{
              L1236.solved_fraction_subpeak <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, i)
            }

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_SUBPEAK, L1236.solved_fraction_subpeak$root) -> L1236.grid_elec_supply

            L1236.grid_elec_supply %>%
              calc_non_segment_frac(i, gcamchina.ELEC_SEGMENT_BASE) -> L1236.non_base

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamchina.ELEC_SEGMENT_BASE, 1 - L1236.non_base) -> L1236.grid_elec_supply

            #Solve for peak
            L1236.solved_fraction_peak <- root_finder(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_PEAK, i)

            if(L1236.solved_fraction_peak>1){
              #print(L1236.region)
              #print(segment_year)
              #print(i)
              #print("Warning peak demand could not solve. So, setting it to a pre-determined value")
              L1236.solved_fraction_peak$root=0.01
            }else{
              L1236.solved_fraction_peak <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_PEAK, i)

            }

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_PEAK, L1236.solved_fraction_peak$root) -> L1236.grid_elec_supply

            L1236.grid_elec_supply %>%
              calc_non_segment_frac(i, gcamusa.ELEC_SEGMENT_BASE) -> L1236.non_int

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_BASE, 1 - L1236.non_int) -> L1236.grid_elec_supply

          }

        }

        if (segment_year %in% c(2005)){

          for (i in unique(dominant_fuels$fuel)){

            # region specific adjustment
            # Part 1: reduce some hydro in base for coal
            if (L1236.region %in% c("Central China Grid")){
              L1236.grid_elec_supply %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_BASE, 0.5) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_INT, 0.5)  -> L1236.grid_elec_supply}
            #
            # # Part 2 : China Southern Power Grid and East China Grid adjustment, creating space for coal in subpeak/peak
            if (L1236.region %in% c("China Southern Power Grid", "East China Grid")){
              L1236.grid_elec_supply %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_BASE, 0.7) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_INT, 0.3) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, 0) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 0) -> L1236.grid_elec_supply}

            #Solve for intermediate
            L1236.solved_fraction_int <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamchina.ELEC_SEGMENT_INT, i)

            if(L1236.solved_fraction_int$root>1){
              #print(L1236.region)
              #print(segment_year)
              #print(i)
              #print("Warning intermediate demand could not solve. So, setting it to a pre-determined value")
              L1236.solved_fraction_int$root=0
            }else{
              L1236.solved_fraction_int <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamchina.ELEC_SEGMENT_INT, i)
            }

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamchina.ELEC_SEGMENT_INT, L1236.solved_fraction_int$root) -> L1236.grid_elec_supply

            L1236.grid_elec_supply %>%
              calc_non_segment_frac(i, gcamusa.ELEC_SEGMENT_BASE) -> L1236.non_base

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_BASE, 1 - L1236.non_base) -> L1236.grid_elec_supply

            #Solve for sub-peak
            L1236.solved_fraction_subpeak <- root_finder(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, i)

            if(L1236.solved_fraction_subpeak>1){
              # these print statements are helpful to identify which fuel/year/region is not fully solved for fuel allocation
              #print(L1236.region)
              #print(segment_year)
              #print(i)
              #print("Warning subpeak demand could not solve. So, setting it to a pre-determined value")
              L1236.solved_fraction_subpeak$root=0
            }else{
              L1236.solved_fraction_subpeak <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, i)
            }

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_SUBPEAK, L1236.solved_fraction_subpeak$root) -> L1236.grid_elec_supply

            L1236.grid_elec_supply %>%
              calc_non_segment_frac(i, gcamchina.ELEC_SEGMENT_BASE) -> L1236.non_base

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamchina.ELEC_SEGMENT_BASE, 1 - L1236.non_base) -> L1236.grid_elec_supply

            #Solve for peak
            L1236.solved_fraction_peak <- root_finder(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_PEAK, i)

            if(L1236.solved_fraction_peak>1){
              # these print statements are helpful to identify which fuel/year/region is not fully solved for fuel allocation
              #print(L1236.region)
              #print(segment_year)
              #print(i)
              #print("Warning peak demand could not solve. So, setting it to a pre-determined value")
              L1236.solved_fraction_peak$root=0.01
            }else{
              L1236.solved_fraction_peak <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_PEAK, i)
            }

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_PEAK, L1236.solved_fraction_peak$root) -> L1236.grid_elec_supply

            L1236.grid_elec_supply %>%
              calc_non_segment_frac(i, gcamusa.ELEC_SEGMENT_BASE) -> L1236.non_int

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_BASE, 1 - L1236.non_int) -> L1236.grid_elec_supply

          }

        }

        if (segment_year %in% c(2010)){

          for (i in unique(dominant_fuels$fuel)){

            # region specific adjustment
            # Part 1: China Southern Power Grid, allocate more fuels to base/int so that coal can to peak/subpeak
            if (L1236.region %in% c("China Southern Power Grid")){
              L1236.grid_elec_supply %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_BASE, 0.6) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, 0.3) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, 0.1) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_BASE, 0.5) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_INT, 0.5) -> L1236.grid_elec_supply}
            #
            # # Part 2 : East China Grid adjustment
            if (L1236.region %in% c("East China Grid")){
              L1236.grid_elec_supply %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_BASE, 0.7) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, 0.3) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, 0) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_BASE, 0.7) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_INT, 0.3) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, 0) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 0) -> L1236.grid_elec_supply}

            # # Part 3 : Central China Grid
            if (L1236.region %in% c("Central China Grid")){
              L1236.grid_elec_supply %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_BASE, 0.6) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_INT, 0.4) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_SUBPEAK, 0) %>%
                replace_fraction("gas", gcamusa.ELEC_SEGMENT_PEAK, 0) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_BASE, 0.6) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_INT, 0.4) %>%
                replace_fraction("hydro", gcamusa.ELEC_SEGMENT_SUBPEAK, 0) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_BASE, 0.5) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_INT, 0.5) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_SUBPEAK, 0) %>%
                replace_fraction("refined liquids", gcamusa.ELEC_SEGMENT_PEAK, 0) -> L1236.grid_elec_supply}

            #Solve for intermediate
            L1236.solved_fraction_int <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamchina.ELEC_SEGMENT_INT, i)

            if(L1236.solved_fraction_int$root>1){
              #print(L1236.region)
              #print(segment_year)
              #print(i)
              #print("Warning intermediate demand could not solve. So, setting it to a pre-determined value")
              L1236.solved_fraction_int$root=0
            }else{
              L1236.solved_fraction_int <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamchina.ELEC_SEGMENT_INT, i)
            }

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamchina.ELEC_SEGMENT_INT, L1236.solved_fraction_int$root) -> L1236.grid_elec_supply

            L1236.grid_elec_supply %>%
              calc_non_segment_frac(i, gcamusa.ELEC_SEGMENT_BASE) -> L1236.non_base

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_BASE, 1 - L1236.non_base) -> L1236.grid_elec_supply

            #Solve for sub-peak
            L1236.solved_fraction_subpeak <- root_finder(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, i)

            if(L1236.solved_fraction_subpeak>1){
              # these print statements are helpful to identify which fuel/year/region is not fully solved for fuel allocation
              #print(L1236.region)
              #print(segment_year)
              #print(i)
              #print("Warning subpeak demand could not solve. So, setting it to a pre-determined value")
              L1236.solved_fraction_subpeak$root=0
            }else{
              L1236.solved_fraction_subpeak <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, i)
            }

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_SUBPEAK, L1236.solved_fraction_subpeak$root) -> L1236.grid_elec_supply

            L1236.grid_elec_supply %>%
              calc_non_segment_frac(i, gcamchina.ELEC_SEGMENT_BASE) -> L1236.non_base

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamchina.ELEC_SEGMENT_BASE, 1 - L1236.non_base) -> L1236.grid_elec_supply

            #Solve for peak
            L1236.solved_fraction_peak <- root_finder(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_PEAK, i)

            if(L1236.solved_fraction_peak>1){
              # these print statements are helpful to identify which fuel/year/region is not fully solved for fuel allocation
              #print(L1236.region)
              #print(segment_year)
              #print(i)
              #print("Warning peak demand could not solve. So, setting it to a pre-determined value")
              L1236.solved_fraction_peak$root=0.01
            }else{
              L1236.solved_fraction_peak <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_PEAK, i)
            }

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_PEAK, L1236.solved_fraction_peak$root) -> L1236.grid_elec_supply

            L1236.grid_elec_supply %>%
              calc_non_segment_frac(i, gcamusa.ELEC_SEGMENT_BASE) -> L1236.non_int

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_BASE, 1 - L1236.non_int) -> L1236.grid_elec_supply

          }

        }

      }}

    # Re-join data for non calibrated years
    # Ensure that generation = total generation * calibrated load segment fuel fraction
    # 5th NOV 2025,xsl add based on the gcamusa

    #L1236.grid_elec_supply %>%
    #  bind_rows(L1236.grid_elec_supply_non_cal) %>%
    #  mutate(generation = tot_generation * fraction) -> L1236.grid_elec_supply

    L1236.grid_elec_supply %>%
      select(-tot_generation, -generation) %>%
      group_by(grid_region, segment, fuel) %>%
      tidyr::complete(year = MODEL_BASE_YEARS) %>%
      mutate(fraction = approx_fun(year, fraction, rule = 2)) %>%
      ungroup() %>%
      filter(!(year %in% gcamchina.LOAD_SEG_CAL_YEARS)) %>%
      left_join_error_no_match(L1236.grid_elec_supply_non_cal %>% select(-fraction, -generation),
                               by = c("grid_region", "segment", "fuel", "year")) %>%
      mutate(generation = tot_generation * fraction) %>%
      bind_rows(L1236.grid_elec_supply) -> L1236.grid_elec_supply

    L1235.elecS_demand_fraction_CHINA %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) ->
      L1236.elecS_demand_fraction_adj_CHINA
    FILLED_MODEL_YEARS <- setdiff(MODEL_BASE_YEARS, gcamchina.LOAD_SEG_CAL_YEARS)
    if(length(FILLED_MODEL_YEARS) > 0) {
      warning("Not solving non gcamchina.LOAD_SEG_CAL_YEARS years and extending forward, load shape is being adjusted to match")

      L1235.elecS_horizontal_vertical_GCAM_coeff_CHINA %>%
        select(grid_region, vert.segment = supplysector, horiz.segment = minicam.energy.input, horiz.coef = coefficient) %>%
        left_join_error_no_match(L1235.elecS_demand_fraction_CHINA, by=c("grid_region", "vert.segment" = "vertical_segment")) %>%
        rename(vert.coef = demand_fraction) ->
        orig_shape
      L1236.grid_elec_supply %>%
        filter(year %in% FILLED_MODEL_YEARS) %>%
        group_by(grid_region, year, segment) %>%
        summarize(horiz.gen = sum(generation)) %>%
        mutate(gr.gen = sum(horiz.gen)) %>%
        ungroup() ->
        actual_gen

      optim_load_match <- function(new.coef, initial_coef, target_df) {
        # set the trial vertical coefficients and calculate how different
        # the calculated generation by horizontal load segment is from our
        # target which is bottom up from our fuel appropriations
        initial_coef %>%
          mutate(vert.coef = new.coef) %>%
          left_join_error_no_match(target_df, ., by=c("vert.segment")) %>%
          group_by(horiz.segment) %>%
          summarize(target = unique(horiz.gen),
                    calc = sum(gr.gen * horiz.coef * vert.coef),
                    error = (target - calc)/target) %>%
          summarize(error = sum(error * error)) %>% pull(error)
      }
      set_coef_group <- function(group_df) {
        # split out just the vertical coefficients which is what we are going to
        # try to solve new values for
        group_df %>%
          select(vert.segment, vert.coef) %>%
          distinct() ->
          initial_coef
        target_df <- select(group_df, -vert.coef)
        # solve for new vertical coefficients
        # we give it the initial_coef so that we can match back the vertical segment names
        # and of course the rest of the data to re-calculate the to-down estimation of generation
        # by horizontal load segment and of course our target genration values
        optim_out <- optim(initial_coef$vert.coef, optim_load_match, gr = "BFGS", initial_coef, target_df)

        # return whatever the solver came up as well as the convergence status so we can perform
        # error checking later
        initial_coef %>%
          mutate(vert.coef = optim_out$par,
                 convergence = optim_out$convergence)
      }

      orig_shape %>%
        left_join(actual_gen, by=c("grid_region", "horiz.segment" = "segment")) %>%
        group_by(grid_region, horiz.segment, year) %>%
        mutate(implied.gen = sum(gr.gen * horiz.coef * vert.coef)) %>%
        ungroup() %>%
        tidyr::nest(data = -c("grid_region", "year")) %>%
        mutate(data = lapply(data, set_coef_group)) %>%
        tidyr::unnest(c(data)) %>%
        rename(vertical_segment = vert.segment, demand_fraction = vert.coef) ->
        adjusted_vertical_coefs

      assertthat::assert_that(filter(adjusted_vertical_coefs, convergence != 0) %>% nrow() == 0,
                              msg = "Failed to find a new set of vertical demand coef in some region / years")

      L1236.elecS_demand_fraction_adj_CHINA %>%
        anti_join(adjusted_vertical_coefs, by = c("grid_region", "vertical_segment", "year")) %>%
        bind_rows(adjusted_vertical_coefs %>% select(-convergence)) ->
        L1236.elecS_demand_fraction_adj_CHINA
    }



    # ===================================================

    # Produce outputs

    L1236.grid_elec_supply %>%
      add_title("Electricity supply by fuel by horizontal load segment in each grid region.") %>%
      add_units("EJ; unitless (fraction)") %>%
      add_comments("Electricity supply by fuel by horizontal load segment in each grid region.") %>%
      add_comments("Based on calculated fraction of fuel in the horizontal load segments.") %>%
      add_legacy_name("L1236.grid_elec_supply") %>%
      add_precursors("L1234.out_EJ_grid_elec_F_CHINA",
                     "L1235.grid_elec_supply_CHINA",
                     "L1235.elecS_demand_fraction_CHINA",
                     "L1235.elecS_horizontal_vertical_CHINA",
                     "gcam-china/elecS_horizontal_to_vertical_map") ->
      L1236.grid_elec_supply_CHINA

    L1236.elecS_demand_fraction_adj_CHINA %>%
      add_title("A potentially adjusted version of L1235.elecS_demand_fraction_CHINA") %>%
      add_units("unitless (fraction)") %>%
      add_comments("In the case of model years not in gcamchina.LOAD_SEG_CAL_YEARS we need to") %>%
      add_comments("extend the fuel to segment attribution, however that will result in inconsistent") %>%
      add_comments("energy totals by segment, so we adjust the vertical coefficients to compensate") %>%
      same_precursors_as(L1236.grid_elec_supply_CHINA) %>%
      add_precursors("L1235.elecS_horizontal_vertical_GCAM_coeff_CHINA") ->
      L1236.elecS_demand_fraction_adj_CHINA

    return_data(L1236.grid_elec_supply_CHINA,L1236.elecS_demand_fraction_adj_CHINA)

  } else {
    stop("Unknown command")
  }
}
