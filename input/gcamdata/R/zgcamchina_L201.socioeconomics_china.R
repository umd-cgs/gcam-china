# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L201.socioeconomics
#'
#' Interest rate, population, labor productivity, and GDP for GCAM-China.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L201.InterestRate_CHINA}, \code{L201.Pop_GCAMCHINA}, \code{L201.BaseGDP_GCAMCHINA}, \code{L201.LaborForceFillout_CHINA}, \code{L201.LaborProductivity_GCAMCHINA}. The corresponding file in the
#' original data system was \code{L201.socioeconomics_CHINA.R} (gcam-China level2).
#' @details Interest rate, population, labor productivity, and GDP for GCAM-China.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YangLiu Aug 2018 / YangOu Dec 2023 /YangLiu Sep 2024
module_gcamchina_L201.socioeconomics <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             "L101.Pop_thous_province",
             "L101.GDP_mil90usd_province"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L201.Pop_GCAMCHINA",
             "L201.GDP_GCAMCHINA",
             "L201.Pop_national_updated_China",
             "L201.GDP_national_updated_China"))
  } else if(command == driver.MAKE) {

    # silence package checks
    year <- value <- province <- totalPop <- baseGDP <- pop <- growth <- timestep <- GDP <- region <- pcGDP <- output <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L101.Pop_thous_province <- get_data(all_data, "L101.Pop_thous_province", strip_attributes = TRUE)
    L101.GDP_mil90usd_province <- get_data(all_data, "L101.GDP_mil90usd_province", strip_attributes = TRUE)

    # ===================================================
    # L201.InterestRate: Interest rates by region
    L201.InterestRate <- tibble(region = gcamchina.PROVINCES_NOHKMC, interest.rate = socioeconomics.DEFAULT_INTEREST_RATE)

    # L201.Pop_GCAMChina: Population by region, downscaled based on UN population projection
    L201.Pop_GCAMCHINA <- L101.Pop_thous_province %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(totalPop = pop,
             region = province) %>%
      mutate(totalPop = round(totalPop, socioeconomics.POP_DIGITS))

    # L201.GDP_GCAMChina: Base GDP for GCAM-China reference scenario
    L201.GDP_GCAMCHINA <- L101.GDP_mil90usd_province %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(GDP = value,
             region = province) %>%
      mutate(GDP = round(GDP, socioeconomics.GDP_DIGITS))

    # Add China-region updates
    # Updated China-region population
    L201.Pop_GCAMCHINA %>%
      group_by(year) %>%
      summarise(totalPop = sum(totalPop)) %>%
      ungroup() %>%
      mutate(totalPop = round(totalPop, socioeconomics.POP_DIGITS),
             region = gcamchina.REGION) %>%
      select(region, year, totalPop) -> L201.Pop_national_updated_China

    # Updated China-region base GDP
    L201.GDP_GCAMCHINA %>%
      group_by(year) %>%
      summarise(GDP = sum(GDP)) %>%
      ungroup() %>%
      mutate(GDP = round(GDP, socioeconomics.GDP_DIGITS),
             region = gcamchina.REGION) %>%
      select(region, year, GDP) -> L201.GDP_national_updated_China



    # ===================================================

    # Produce outputs
    L201.Pop_GCAMCHINA %>%
      add_title("Population by province") %>%
      add_units("thousand persons") %>%
      add_comments("Data from L101.Pop_thous_province") %>%
      add_legacy_name("L201.Pop_GCAMChina") %>%
      add_precursors("L101.Pop_thous_province") ->
      L201.Pop_GCAMCHINA

    L201.GDP_GCAMCHINA %>%
      add_title("Base year GDP by province") %>%
      add_units("million 1990 USD") %>%
      add_comments("Data from L101.GDP_mil90usd_province") %>%
      add_legacy_name("L201.GDP_GCAMChina") %>%
      add_precursors("L101.GDP_mil90usd_province") ->
      L201.GDP_GCAMCHINA

    L201.Pop_national_updated_China %>%
      add_title("Updated population for China region, consistent with sum-of-provinces") %>%
      add_units("thousand persons") %>%
      add_comments("Updates China region population to match the 31 provinces total") %>%
      add_legacy_name("L201.Pop_updated_China_national") %>%
      same_precursors_as("L201.Pop_GCAMCHINA") ->
      L201.Pop_national_updated_China

    L201.GDP_national_updated_China %>%
      add_title("Updated base year GDP for China region, consistent with sum-of-provinces") %>%
      add_units("million 1990 USD") %>%
      add_comments("Updates China region base year GDP to match the 31 provinces total") %>%
      same_precursors_as("L201.GDP_GCAMCHINA") ->
      L201.GDP_national_updated_China


    return_data( L201.Pop_GCAMCHINA, L201.GDP_GCAMCHINA, L201.Pop_national_updated_China, L201.GDP_national_updated_China)
  } else {
    stop("Unknown command")
  }
}
