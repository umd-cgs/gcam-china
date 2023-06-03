#' module_gcam.china_LA100.Socioeconomics
#'
#' This chunk generates the historical and future GDP, population and per-capita GDP by province.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.pcGDP_thous90usd_province}, \code{L100.GDP_mil90usd_province}, \code{L100.Pop_thous_province}. The corresponding file in the
#' original data system was \code{LA100.Socioeconomics.R} (gcam-china level1).
#' @details This chunk processes historical and future GDP and population data by province,
#' fill missing values and covert units; and then calculates per capita GDP by province as
#' GDP divided by population.
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @import dplyr
#' @importFrom tidyr gather spread
#' @author RC Jul 2018
module_gcam.china_LA100.Socioeconomics <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "gcam-china/population_thous_province",
             FILE = "gcam-china/gdp_mil10usd_province"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.pcGDP_thous90usd_province",
             "L100.GDP_mil90usd_province",
             "L100.Pop_thous_province"))
  } else if(command == driver.MAKE) {

    year <- GDP <- province.name <- province <- pop <- pcGDP <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    province_names_mappings   <- get_data(all_data, "gcam-china/province_names_mappings")
    population_thous_province <- get_data(all_data, "gcam-china/population_thous_province")
    gdp_mil10usd_province     <- get_data(all_data, "gcam-china/gdp_mil10usd_province")

    # Get a list of the years for socioeconomic variables
    socio_hist_years <- as.integer(grep("[0-9]{4}", names(gdp_mil10usd_province), value = TRUE))

    # Fill missing values in GDP, and convert dollar year
    # Note that missing values are interpolated if bounded and filled with the next closest value otherwise.
    gdp_mil10usd_province %>%
      gather(year, GDP, matches("[0-9]{4}")) %>%
      mutate(year = as.integer(year)) %>%
      # Complete data with all provinces and years and fill missing values
      complete(nesting(province.name), year = socio_hist_years) %>%
      arrange(province.name, year) %>%
      group_by(province.name) %>%
      mutate(GDP = approx_fun(year, GDP, rule = 2),
             # Convert 2010 USD to 1990 USD
             GDP = GDP * gdp_deflator(1990, base_year = 2010)) %>%
      ungroup %>%
      # Replace full provice names with abbreviation
      map_province_name(province_names_mappings, "province", TRUE) ->
      L100.GDP_mil90usd_province


    # Fill missing values in population
    # Note that missing values are interpolated if bounded and filled  with the next closest value otherwise.
    population_thous_province %>%
      gather(year, pop, matches("[0-9]{4}")) %>%
      mutate(year = as.integer(year)) %>%
      # Complete data with all provinces and years and fill missing values
      complete(nesting(province.name), year = socio_hist_years) %>%
      arrange(province.name, year) %>%
      group_by(province.name) %>%
      mutate(pop = approx_fun(year, pop, rule = 2)) %>%
      ungroup %>%
      # Replace full province names with abbreviation
      map_province_name(province_names_mappings, "province", TRUE) ->
      L100.Pop_thous_province

    # Calculate GDP per capita
    L100.GDP_mil90usd_province %>%
      left_join_error_no_match(L100.Pop_thous_province, by = c("province", "year")) %>%
      mutate(pcGDP = GDP / pop) %>%
      replace_na(list(pcGDP = 0)) %>%
      select(province, year, pcGDP) ->
      L100.pcGDP_thous90usd_province

    # Produce outputs
    L100.GDP_mil90usd_province %>%
      add_title("GDP by province") %>%
      add_units("million 1990 USD") %>%
      add_comments("Missing values are interpolated if bounded and filled with the next closest value otherwise") %>%
      add_comments("Units are converted to constant 1990 USD") %>%
      add_legacy_name("L100.GDP_mil90usd_province") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/gdp_mil10usd_province") ->
      L100.GDP_mil90usd_province

    L100.Pop_thous_province %>%
      add_title("Population by province") %>%
      add_units("thousand persons") %>%
      add_comments("Missing values are interpolated if bounded and filled with the next closest value otherwise") %>%
      add_legacy_name("L100.Pop_thous_province") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/population_thous_province") ->
      L100.Pop_thous_province

    L100.pcGDP_thous90usd_province %>%
      add_title("Per-capita GDP by province") %>%
      add_units("thousand 1990 USD per capita") %>%
      add_comments("Calculated as GDP divided by population") %>%
      add_legacy_name("L100.pcGDP_thous90usd_province") %>%
      same_precursors_as("L100.GDP_mil90usd_province") %>%
      same_precursors_as("L100.Pop_thous_province") ->
      L100.pcGDP_thous90usd_province

    return_data(L100.pcGDP_thous90usd_province, L100.GDP_mil90usd_province, L100.Pop_thous_province)
  } else {
    stop("Unknown command")
  }
}
