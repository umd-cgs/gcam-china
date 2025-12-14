# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_exoShutdownScalar_xml
#'
#' Construct XML data structure for \code{policy_exoShutdownScalar.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_exoShutdownScalar.xml}.
module_policy_exoShutdownScalar_xml <- function(command, ...) {
  all_xml_names <- get_xml_names("gcam-china/A_ExoShutdownScalar.csv", "policy_exoShutdownScalar.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(c("L307.exoShutdownScalar"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L307.exoShutdownScalar <- get_data(all_data, "L307.exoShutdownScalar")
    # ===================================================
    # Produce outputs
    for (xml_name in all_xml_names){
      L307.exoShutdownScalar_tmp <- L307.exoShutdownScalar %>%
        filter(xml == xml_name)%>%
        select(-xml)

      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(L307.exoShutdownScalar_tmp, "ExoShutdown",column_order_lookup = NULL) %>%
               add_precursors("L307.exoShutdownScalar")
      )
    }

    # Need this for loop because having issues with lapply(all_xml_names, get)
    list_of_xmls <- list()
    for(xml_name in all_xml_names){
      list_of_xmls[[xml_name]] <- get(xml_name)
    }
    return_multiple_xmls(list_of_xmls, all_xml_names)
  } else {
    stop("Unknown command")
  }
}
