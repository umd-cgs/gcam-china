# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_socioeconomics_xml
#'
#' Construct XML data structure for \code{socioeconomics_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_socioeconomics_CHINA.xml} (gcamchina XML).
module_gcamchina_socioeconomics_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L201.Pop_GCAMCHINA",
             "L201.GDP_GCAMCHINA",
             "L201.Pop_national_updated_China",
             "L201.GDP_national_updated_China",
             FILE = "gcam-china/L281.GlobalTechAccountOutputUseBasePrice_fd_China"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "socioeconomics_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L201.Pop_GCAMCHINA <- get_data(all_data, "L201.Pop_GCAMCHINA")
    L201.GDP_GCAMCHINA <- get_data(all_data, "L201.GDP_GCAMCHINA")
    L201.Pop_national_updated_China <- get_data(all_data, "L201.Pop_national_updated_China")
    L201.GDP_national_updated_China <- get_data(all_data, "L201.GDP_national_updated_China")
    L281.GlobalTechAccountOutputUseBasePrice_fd_China  <- get_data(all_data, "gcam-china/L281.GlobalTechAccountOutputUseBasePrice_fd_China")
    # ===================================================

    # Produce outputs
    create_xml("socioeconomics_CHINA.xml") %>%
      add_xml_data(L201.Pop_GCAMCHINA, "Pop") %>%
      add_xml_data(L201.GDP_GCAMCHINA, "GDP") %>%
      add_xml_data(L201.Pop_national_updated_China, "Pop") %>%
      add_xml_data(L201.GDP_national_updated_China, "GDP") %>%
      #add_xml_data(L281.GlobalTechAccountOutputUseBasePrice_fd_China, "GlobalTechAccountOutputUseBasePrice") %>%
      add_precursors("L201.Pop_GCAMCHINA", "L201.GDP_GCAMCHINA", "L201.Pop_national_updated_China", "L201.GDP_national_updated_China") ->
      socioeconomics_CHINA.xml

    return_data(socioeconomics_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
