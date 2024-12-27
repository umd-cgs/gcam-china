#' module_gcamchina_batch_ghg_emissions_CHINA_xml
#'
#' Construct XML data structure for \code{ghg_emissions_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ghg_emissions_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_ghg_emissions_CHINA.xml} (gcamchina XML).
module_gcamchina_batch_ghg_emissions_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L273.en_ghg_tech_coeff_CHINA",
             "L273.en_ghg_emissions_CHINA",
             "L273.out_ghg_emissions_CHINA",
             "L273.MAC_higwp_CHINA",
             "L273.MAC_higwp_TC_CHINA",
             "L273.MAC_higwp_phaseInTime_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ghg_emissions_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    emiss.coeff <- output.emissions <- NULL

    # Load required inputs
    L273.en_ghg_tech_coeff_CHINA     <- get_data(all_data, "L273.en_ghg_tech_coeff_CHINA")
    L273.en_ghg_emissions_CHINA      <- get_data(all_data, "L273.en_ghg_emissions_CHINA")
    L273.out_ghg_emissions_CHINA     <- get_data(all_data, "L273.out_ghg_emissions_CHINA")
    L273.MAC_higwp_CHINA             <- get_data(all_data, "L273.MAC_higwp_CHINA")
    L273.MAC_higwp_TC_CHINA          <- get_data(all_data, "L273.MAC_higwp_TC_CHINA")
    L273.MAC_higwp_phaseInTime_CHINA <- get_data(all_data, "L273.MAC_higwp_phaseInTime_CHINA")

    # ===================================================
    # Rename to meet header requriements
    L273.en_ghg_tech_coeff_CHINA <- rename(L273.en_ghg_tech_coeff_CHINA, emiss.coef = emiss.coeff)
    L273.out_ghg_emissions_CHINA <- rename(L273.out_ghg_emissions_CHINA, input.emissions = output.emissions)

    # Produce outputs
    create_xml("ghg_emissions_CHINA.xml") %>%
      add_xml_data(L273.en_ghg_tech_coeff_CHINA, "InputEmissCoeff") %>%
      add_xml_data(L273.en_ghg_emissions_CHINA, "InputEmissions") %>%
      add_xml_data(L273.out_ghg_emissions_CHINA, "StbTechOutputEmissions") %>%
      add_xml_data(L273.MAC_higwp_CHINA, "MAC") %>%
      add_xml_data(L273.MAC_higwp_TC_CHINA, "MACTC") %>%
      add_xml_data(L273.MAC_higwp_phaseInTime_CHINA, "MACPhaseIn") %>%
      add_precursors("L273.en_ghg_tech_coeff_CHINA",
                     "L273.en_ghg_emissions_CHINA",
                     "L273.out_ghg_emissions_CHINA",
                     "L273.MAC_higwp_CHINA",
                     "L273.MAC_higwp_TC_CHINA",
                     "L273.MAC_higwp_phaseInTime_CHINA") ->
      ghg_emissions_CHINA.xml

    return_data(ghg_emissions_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
