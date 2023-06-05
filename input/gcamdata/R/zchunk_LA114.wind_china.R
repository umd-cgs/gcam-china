#' module_gcamchina_LA114.Wind
#'
#' Compute capacity factors for wind by China province.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L114.CapacityFactor_wind_province}. The corresponding file in the
#' original data system was \code{LA114.Wind.R} (gcam-china level1).
#' @details Computed from A23 tables for capital, variable and fixed wind costs by province.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author Liu July 2018
module_gcamchina_LA114.Wind <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( FILE = "gcam-china/wind_potential_province",
              "L113.globaltech_OMfixed_ATB",
              "L113.globaltech_OMvar_ATB",
              "L113.globaltech_capital_ATB",
              FILE = "gcam-china/province_names_mappings"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L114.CapacityFactor_wind_province"))
  } else if(command == driver.MAKE) {

    technology <- year <- state <- sector <- capacity.factor <- fuel <- value <- base_cost <-
      region <- province.name <- base.cost <- NULL  # silence package check.

    all_data <- list(...)[[1]]

    # -----------------------------------------------------------------------------
    # 1.Load required inputs

    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings")
    wind_potential_province <- get_data(all_data, "gcam-china/wind_potential_province", strip_attributes = T)
    L113.globaltech_capital_ATB <- get_data(all_data, "L113.globaltech_capital_ATB")
    L113.globaltech_OMfixed_ATB <- get_data(all_data, "L113.globaltech_OMfixed_ATB")
    L113.globaltech_OMvar_ATB <- get_data(all_data, "L113.globaltech_OMvar_ATB")

    # -----------------------------------------------------------------------------
    # 2.perform computations

    # This function filters L113 tables for wind, then gathers and interpolates...
    # ... to get a single value for wind base cost year. Note that the interpolation is...
    # ... redundant whilst gcamchina.WIND_BASE_COST_YEAR = 2005, ...
    # ... since 2005 is an existing data point in all L113 tables.
    filter_gather_interp_get_cost <- function(x) {
      . <- NULL  # silence package check notes
      x %>% filter(technology == "wind") %>%
        gather_years %>%
        select(year, value) %>%
        complete(year = c(year, gcamchina.WIND_BASE_COST_YEAR)) %>%
        mutate(value = approx_fun(year, value, rule = 2)) %>%
        filter(year == gcamchina.WIND_BASE_COST_YEAR) %>%
        pull(value)
    }

    L113.globaltech_capital_ATB %>% filter_gather_interp_get_cost -> L114.CapCost
    L113.globaltech_OMfixed_ATB %>% filter_gather_interp_get_cost -> L114.OMFixedCost
    L113.globaltech_OMvar_ATB %>% filter_gather_interp_get_cost -> L114.OMVarCost
    # ^^ all above rates are in $1975

    # Get fixed charge rate of capital for wind
    filter(L113.globaltech_capital_ATB, technology == "wind")$fixed.charge.rate -> L114.FixedChargeRate

    wind_potential_province %>%
      mutate(sector = "electricity generation",
             fuel = "wind",
             # capacity factor computed dividing sum of capital and fixed costs (converted to 1975$/GJ) by base cost minus variable cost (converted to 1975$/GJ)
             base.cost = base.cost * gdp_deflator(1975, 2007) / CONV_KWH_GJ,
             # Calculate the capacity factor for the base wind turbine in each province
             capacity.factor = (L114.CapCost * L114.FixedChargeRate + L114.OMFixedCost) /
               (CONV_KWH_GJ * CONV_YEAR_HOURS) / (base.cost - (L114.OMVarCost / (1000 * CONV_KWH_GJ)))) %>%
      select(province.name, sector, fuel, capacity.factor) %>%
      # Taiwan is not included in the output, drop it here to avoid NAs and to use the map_province_name function.
      filter(province.name != "Taiwan") %>%
      map_province_name(province_names_mappings, "province", TRUE) ->
      L114.CapacityFactor_wind_province

    # -----------------------------------------------------------------------------
    # 3.Produce outputs
    L114.CapacityFactor_wind_province %>%
      add_title("Capacity factor for wind by province") %>%
      add_units("Unitless") %>%
      add_comments("Computed from A23 tables for capital, variable and fixed wind costs") %>%
      add_legacy_name("L114.CapacityFactor_wind_province") %>%
      add_precursors("gcam-china/wind_potential_province",
                     "gcam-china/province_names_mappings",
                     "L113.globaltech_capital_ATB",
                     "L113.globaltech_OMfixed_ATB",
                     "L113.globaltech_OMvar_ATB") ->
      L114.CapacityFactor_wind_province

    return_data(L114.CapacityFactor_wind_province)
  } else {
    stop("Unknown command")
  }
}
