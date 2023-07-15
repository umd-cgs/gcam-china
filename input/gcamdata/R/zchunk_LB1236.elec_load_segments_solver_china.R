# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_LB1236.elec_load_segments_solver_china
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
#' @author MTB August 2018 / YangOu July 2023 / Kanishka Narayan July 2023
module_gcamchina_LB1236.elec_load_segments_solver_china <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/elecS_horizontal_to_vertical_map",
             "L1234.out_EJ_grid_elec_F_CHINA",
             "L1235.grid_elec_supply_CHINA",
             "L1235.elecS_demand_fraction_CHINA",
             "L1235.elecS_horizontal_vertical_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1236.grid_elec_supply_CHINA"))
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
      summarise(tot_generation = sum(generation)) %>%
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
                                 summarise(tot_generation = sum(generation)) %>%
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
        summarise(non_segment_frac = sum(fraction)) %>%
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
        summarise(generation = sum(generation)) %>%
        ungroup() -> L1236.grid_check

      # Calculate electricity demand for each horizontal segment in each grid region
      L1236.grid_elec_supply %>%
        group_by(grid_region, year) %>%
        summarise(tot_demand = sum(generation)) %>%
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


    # Calculate total electricity generation by year / grid region
    L1236.out_EJ_grid_elec_F %>%
      group_by(grid_region, sector, year) %>%
      summarise(grid_total = sum(tot_generation)) %>%
      ungroup() -> L1236.grid_total

    # Calculate the share of generation from a given fuel across load segment by year / grid region
    L1236.out_EJ_grid_elec_F %>%
      left_join_error_no_match(L1236.grid_total, by = c("grid_region", "sector", "year")) %>%
      mutate(grid_share_fuel = tot_generation / grid_total) -> L1236.out_EJ_grid_elec_F

    # For each grid region and year, calculate the fraction of electricity generation by fuel by horizontal load segment such that
    # electricity supplies and demands balance.
    for (r in seq_along(L1236.gridregion_list)){
      for(y in seq_along(gcamusa.LOAD_SEG_CAL_YEARS)){

        L1236.region <- L1236.gridregion_list[r]
        segment_year <- gcamusa.LOAD_SEG_CAL_YEARS[y]

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

        if (segment_year %in% c(2015)){

          for (i in unique(dominant_fuels$fuel)){

            #Solve for int
            L1236.solved_fraction_int <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamchina.ELEC_SEGMENT_INT, i)

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamchina.ELEC_SEGMENT_INT, L1236.solved_fraction_int$root) -> L1236.grid_elec_supply

            L1236.grid_elec_supply %>%
              calc_non_segment_frac(i, gcamusa.ELEC_SEGMENT_BASE) -> L1236.non_base

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_BASE, 1 - L1236.non_base) -> L1236.grid_elec_supply

            #Solve for sub-peak

            root_finder <- function(func, interval,...) {
              result <- tryCatch(uniroot(func, interval,...), error = function(e) NULL)
              if (!is.null(result))
                return(result$root)
              else
                return(100)
            }

            L1236.solved_fraction <- root_finder(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, i)

            if(L1236.solved_fraction>0.5){
              print("Warning subpeak demand could not solve. So, setting it to a pre-determined value")
              L1236.solved_fraction$root=0.06
            }else{


              L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_SUBPEAK, i)

            }

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_SUBPEAK, L1236.solved_fraction$root) -> L1236.grid_elec_supply

            L1236.grid_elec_supply %>%
              calc_non_segment_frac(i, gcamchina.ELEC_SEGMENT_BASE) -> L1236.non_base

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamchina.ELEC_SEGMENT_BASE, 1 - L1236.non_base) -> L1236.grid_elec_supply

            subpeak_frac <- L1236.solved_fraction$root



            L1236.solved_fraction <- root_finder(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_PEAK, i)

            if(L1236.solved_fraction>0.5){
              print("Warning peak demand could not solve. So, setting it to a pre-determined value")
              L1236.solved_fraction$root=0.01
            }else{


              L1236.solved_fraction <- uniroot(check_elec_segments, c(0, 1), L1236.region, gcamusa.ELEC_SEGMENT_PEAK, i)

            }

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_PEAK, L1236.solved_fraction$root) -> L1236.grid_elec_supply

            L1236.grid_elec_supply %>%
              calc_non_segment_frac(i, gcamusa.ELEC_SEGMENT_BASE) -> L1236.non_int

            L1236.grid_elec_supply %>%
              replace_fraction(i, gcamusa.ELEC_SEGMENT_BASE, 1 - L1236.non_int) -> L1236.grid_elec_supply



          }

        }

        if (segment_year %in% c(2005, 1990,2010)) {
          # For years 2005 & 1990, we use solved fractions from the most recent year as a starting point
          # for calculating the fuel fractions for the current year.
          # Map fractions from prevoius gcamusa.LOAD_SEG_CAL_YEARS to current segment_year.
          L1236.grid_elec_supply %>%
            filter(year == segment_year,
                   grid_region == L1236.region) %>%
            left_join_error_no_match(L1236.grid_elec_supply %>%
                                       filter(year > segment_year) %>%
                                       # NOTE:  can't combine these filters because doing so filters out all entries
                                       filter(year == min(year)) %>%
                                       select(-year, -tot_generation, -generation),
                                     by = c("grid_region", "segment", "fuel")) %>%
            mutate(fraction = fraction.y,
                   generation = tot_generation * fraction) %>%
            select(grid_region, segment, fuel, year, tot_generation, fraction, generation) %>%
            bind_rows(L1236.grid_elec_supply %>%
                        filter(year != segment_year | grid_region != L1236.region)) -> L1236.grid_elec_supply

        }


      }}

    # Re-join data for non calibrated years
    # Ensure that generation = total generation * calibrated load segment fuel fraction
    L1236.grid_elec_supply %>%
      bind_rows(L1236.grid_elec_supply_non_cal) %>%
      mutate(generation = tot_generation * fraction) -> L1236.grid_elec_supply

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

    return_data(L1236.grid_elec_supply_CHINA)

  } else {
    stop("Unknown command")
  }
}
