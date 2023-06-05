#' module_gcamchina_L2231.electricity_nuclear_CHINA
#'
#' Fixoutput of Electricity generation from nuclear by China province
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2231.StubTechFixOut_elec_nuclear_CHINA}. The corresponding file in the
#' original data system was \code{L2231.electricity_nuclear_CHINA.R} (gcam-china level2).
#' @details Fixoutput of Electricity generation from nuclear by China province from 2015 to 2030, adding StubTechnologies
#' @importFrom dplyr mutate select rename left_join
#' @importFrom tidyr gather
#' @author LuRen July 2019
module_gcamchina_L2231.electricity_nuclear_CHINA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "gcam-china/Nuclear_incremental_fixed"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2231.StubTechFixOut_elec_nuclear_CHINA"))
  } else if(command == driver.MAKE) {


    all_data <- list(...)[[1]]

    # Silence package checks
    province.name <- fixedOutput <- region <- supplysector <- subsector <- stub.technology <- share.weight.year <- subs.share.weight <- tech.share.weight <-
      fuel <- year <- value <- province <- NULL

    # Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings")
    fixed_nuclear <- get_data(all_data, "gcam-china/Nuclear_incremental_fixed")

    # ===================================================
    # Fixoutput of Electricity generation from nuclear by China province

    L2231.StubTechFixOut_elec_nuclear_CHINA <- fixed_nuclear %>%
      gather(year, fixedOutput, -province.name) %>%
      # keep only model future years
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      mutate(supplysector = "electricity", subsector = "nuclear", stub.technology = "Gen_III", subs.share.weight = 0, tech.share.weight = 0) %>%
      left_join(province_names_mappings %>% select(province, province.name), by = "province.name") %>%
      mutate(year = as.numeric(substr(year,0,4)), share.weight.year = year) %>%
      select(-province.name) %>%
      rename(region = province) %>%
      mutate(fixedOutput = if_else(fixedOutput < 0, 0, fixedOutput)) %>%
      select(region, supplysector, subsector, stub.technology, year, fixedOutput, share.weight.year, subs.share.weight, tech.share.weight)

    # ===================================================
    # Produce outputs
    L2231.StubTechFixOut_elec_nuclear_CHINA %>%
      add_title("Electricity generation from nuclear by region/supplysector/subsector/stubtechnology") %>%
      add_units("EJ") %>%
      add_comments("L2231.electricity_nuclear_CHINA Fixoutput of Electricity generation from nuclear by China province") %>%
      add_legacy_name("L2231.StubTechFixOut_elec_nuclear_CHINA") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/Nuclear_incremental_fixed") ->
      L2231.StubTechFixOut_elec_nuclear_CHINA

    return_data(L2231.StubTechFixOut_elec_nuclear_CHINA)
  } else {
    stop("Unknown command")
  }
}
