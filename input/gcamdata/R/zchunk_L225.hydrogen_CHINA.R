#' module_gcam.china_L225.hydrogen_china
#'
#' Selects the subsectors to be removed from the hydrogen sectors in GCAM-China
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L225.DeleteSubsector_h2_CHINA}. The corresponding file in the
#' original data system was \code{L225.hydrogen_CHINA.R} (gcam-china level2).
#' @details This chunk selects the subsectors to be removed from the hydrogen sectors in GCAM-China at the national level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YangLiu September 2018
module_gcam.china_L225.hydrogen_china <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L225.SubsectorLogit_h2"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L225.DeleteSubsector_h2_CHINA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    region <- subsector <- supplysector <- NULL  # silence package check notes

    # Load required inputs
    L225.SubsectorLogit_h2 <- get_data(all_data, "L225.SubsectorLogit_h2", strip_attributes = T)

    # ===================================================
    # This chunk selects the subsectors to be removed from the
    # hydrogen sectors in GCAM CHINA at the national level.

    # Since there is no basis for inter-province competition in the hydrogen sector
    # keep the logit exponents for hydrogen at the national level for GCAM CHINA.
    # Select the wind, solar, and electricity subsectors because these resources do
    # not exists at the national level in GCAM CHINA.
    L225.SubsectorLogit_h2 %>%
      # Copy the region column to remove the attributes from the data frame.
      mutate(region = region) %>%
      filter(region == "China", subsector %in% c("electricity")) %>%
      select(region, supplysector, subsector) ->
      L225.DeleteSubsector_h2_CHINA

    # ===================================================

    # Produce outputs
    L225.DeleteSubsector_h2_CHINA %>%
      add_title("Subsector logit exponents of hydrogen sectors in the China to be removed") %>%
      add_units("Unitless") %>%
      add_comments("Select the national subsector logit exponents to be excluded from GCAM CHINA") %>%
      add_legacy_name("L225.DeleteSubsector_h2_CHINA") %>%
      add_precursors("L225.SubsectorLogit_h2") ->
      L225.DeleteSubsector_h2_CHINA

    return_data(L225.DeleteSubsector_h2_CHINA)
  } else {
    stop("Unknown command")
  }
}
