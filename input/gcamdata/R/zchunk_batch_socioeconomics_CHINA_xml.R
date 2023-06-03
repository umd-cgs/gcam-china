#' module_gcamchina_batch_socioeconomics_CHINA_xml
#'
#' Construct XML data structure for \code{socioeconomics_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_socioeconomics_CHINA.xml} (gcamchina XML).
module_gcamchina_batch_socioeconomics_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.Pop_GCAMCHINA",
             "L201.BaseGDP_GCAMCHINA",
             "L201.LaborForceFillout_CHINA",
             "L201.LaborProductivity_GCAMCHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "socioeconomics_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.Pop_GCAMCHINA <- get_data(all_data, "L201.Pop_GCAMCHINA")
    L201.BaseGDP_GCAMCHINA <- get_data(all_data, "L201.BaseGDP_GCAMCHINA")
    L201.LaborForceFillout_CHINA <- get_data(all_data, "L201.LaborForceFillout_CHINA")
    L201.LaborProductivity_GCAMCHINA <- get_data(all_data, "L201.LaborProductivity_GCAMCHINA")

    # ===================================================

    # Produce outputs
    create_xml("socioeconomics_CHINA.xml") %>%
      add_xml_data(L201.Pop_GCAMCHINA, "Pop") %>%
      add_xml_data(L201.BaseGDP_GCAMCHINA, "BaseGDP") %>%
      add_xml_data(L201.LaborForceFillout_CHINA, "LaborForceFillout") %>%
      add_xml_data(L201.LaborProductivity_GCAMCHINA, "LaborProductivity") %>%
      add_precursors("L201.Pop_GCAMCHINA", "L201.BaseGDP_GCAMCHINA", "L201.LaborForceFillout_CHINA", "L201.LaborProductivity_GCAMCHINA") ->
      socioeconomics_CHINA.xml

    return_data(socioeconomics_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
