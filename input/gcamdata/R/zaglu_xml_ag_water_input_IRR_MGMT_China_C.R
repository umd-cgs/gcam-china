# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_ag_water_input_IRR_MGMT_China_C_xml
#'
#' Construct XML data structure for \code{ag_water_input_IRR_MGMT_China_C.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_water_input_IRR_MGMT_China_C.xml}. The corresponding file in the
#' original data system was \code{batch_ag_water_input_IRR_MGMT_xml.R} (aglu XML).
module_aglu_batch_ag_water_input_IRR_MGMT_China_C_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "gcam-china/L2072.AgCoef_IrrBphysWater_ag_mgmt_China_C",
      FILE = "gcam-china/L2072.AgCoef_IrrWaterWdraw_ag_mgmt_China_C",
      FILE = "gcam-china/L2072.AgCoef_IrrWaterCons_ag_mgmt_China_C",
      FILE = "gcam-china/L2072.AgCoef_RfdBphysWater_ag_mgmt_China_C",
      FILE = "gcam-china/L2072.AgCoef_BphysWater_bio_mgmt_China_C",
      FILE = "gcam-china/L2072.AgCoef_IrrWaterWdraw_bio_mgmt_China_C",
      FILE = "gcam-china/L2072.AgCoef_IrrWaterCons_bio_mgmt_China_C")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ag_water_input_IRR_MGMT_China_C.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ===================================================

    # Produce outputs ----
    create_xml("ag_water_input_IRR_MGMT_China_C.xml") %>%
      add_xml_data(L2072.AgCoef_IrrBphysWater_ag_mgmt_China_C, "AgCoef") %>%
      add_xml_data(L2072.AgCoef_IrrWaterWdraw_ag_mgmt_China_C, "AgCoef") %>%
      add_xml_data(L2072.AgCoef_IrrWaterCons_ag_mgmt_China_C, "AgCoef") %>%
      add_xml_data(L2072.AgCoef_RfdBphysWater_ag_mgmt_China_C, "AgCoef") %>%
      add_xml_data(L2072.AgCoef_BphysWater_bio_mgmt_China_C, "AgCoef") %>%
      add_xml_data(L2072.AgCoef_IrrWaterWdraw_bio_mgmt_China_C, "AgCoef") %>%
      add_xml_data(L2072.AgCoef_IrrWaterCons_bio_mgmt_China_C, "AgCoef") %>%
      add_precursors("gcam-china/L2072.AgCoef_IrrBphysWater_ag_mgmt_China_C",
                     "gcam-china/L2072.AgCoef_IrrWaterWdraw_ag_mgmt_China_C",
                     "gcam-china/L2072.AgCoef_IrrWaterCons_ag_mgmt_China_C",
                     "gcam-china/L2072.AgCoef_RfdBphysWater_ag_mgmt_China_C",
                     "gcam-china/L2072.AgCoef_BphysWater_bio_mgmt_China_C",
                     "gcam-china/L2072.AgCoef_IrrWaterWdraw_bio_mgmt_China_C",
                     "gcam-china/L2072.AgCoef_IrrWaterCons_bio_mgmt_China_C") ->
      ag_water_input_IRR_MGMT_China_C.xml

    return_data(ag_water_input_IRR_MGMT_China_C.xml)
  } else {
    stop("Unknown command")
  }
}
