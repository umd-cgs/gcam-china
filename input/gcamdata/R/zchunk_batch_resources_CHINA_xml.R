# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcam.china_batch_resources_CHINA_xml
#'
#' Construct XML data structure for \code{resources_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{resources_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_resources_CHINA_xml.R} (gcamchina XML).
module_gcam.china_batch_resources_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L210.RenewRsrc_CHINA",
             "L210.RenewRsrcPrice_CHINA",
             "L210.UnlimitRsrc_CHINA",
             "L210.UnlimitRsrcPrice_CHINA",
             "L210.SmthRenewRsrcTechChange_CHINA",
             "L210.SmthRenewRsrcCurves_wind_CHINA",
             "L210.SmthRenewRsrcTechChange_offshore_wind_CHINA",
             "L210.SmthRenewRsrcCurves_offshore_wind_CHINA",
             "L210.GrdRenewRsrcCurves_geo_CHINA",
             "L210.GrdRenewRsrcMax_geo_CHINA",
             "L210.SmthRenewRsrcCurvesGdpElast_roofPV_CHINA",
             "L210.ResTechShrwt_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "resources_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L210.RenewRsrc_CHINA <- get_data(all_data, "L210.RenewRsrc_CHINA")
    L210.RenewRsrcPrice_CHINA <- get_data(all_data, "L210.RenewRsrcPrice_CHINA")
    L210.UnlimitRsrc_CHINA <- get_data(all_data, "L210.UnlimitRsrc_CHINA")
    L210.UnlimitRsrcPrice_CHINA <- get_data(all_data, "L210.UnlimitRsrcPrice_CHINA")
    L210.SmthRenewRsrcTechChange_CHINA <- get_data(all_data, "L210.SmthRenewRsrcTechChange_CHINA")
    L210.SmthRenewRsrcTechChange_offshore_wind_CHINA <- get_data(all_data, "L210.SmthRenewRsrcTechChange_offshore_wind_CHINA")
    L210.SmthRenewRsrcCurves_offshore_wind_CHINA <- get_data(all_data, "L210.SmthRenewRsrcCurves_offshore_wind_CHINA")
    L210.SmthRenewRsrcCurves_wind_CHINA <- get_data(all_data, "L210.SmthRenewRsrcCurves_wind_CHINA")
    L210.GrdRenewRsrcCurves_geo_CHINA <- get_data(all_data, "L210.GrdRenewRsrcCurves_geo_CHINA")
    L210.GrdRenewRsrcMax_geo_CHINA <- get_data(all_data, "L210.GrdRenewRsrcMax_geo_CHINA")
    L210.SmthRenewRsrcCurvesGdpElast_roofPV_CHINA <- get_data(all_data, "L210.SmthRenewRsrcCurvesGdpElast_roofPV_CHINA")
    L210.ResTechShrwt_CHINA <- get_data(all_data, "L210.ResTechShrwt_CHINA")

    # ===================================================

    # Produce outputs
    create_xml("resources_CHINA.xml") %>%
      add_xml_data(L210.RenewRsrc_CHINA, "RenewRsrc") %>%
      add_xml_data(L210.RenewRsrcPrice_CHINA, "RenewRsrcPrice") %>%
      add_xml_data(L210.UnlimitRsrc_CHINA, "UnlimitRsrc") %>%
      add_xml_data(L210.UnlimitRsrcPrice_CHINA, "UnlimitRsrcPrice") %>%
      add_xml_data(L210.SmthRenewRsrcTechChange_CHINA, "SmthRenewRsrcTechChange") %>%
      add_xml_data(L210.SmthRenewRsrcTechChange_offshore_wind_CHINA, "SmthRenewRsrcTechChange") %>%
      add_xml_data(L210.SmthRenewRsrcCurves_wind_CHINA, "SmthRenewRsrcCurves") %>%
      add_xml_data(L210.SmthRenewRsrcCurves_offshore_wind_CHINA, "SmthRenewRsrcCurves") %>%
      add_xml_data(L210.GrdRenewRsrcCurves_geo_CHINA, "GrdRenewRsrcCurves") %>%
      add_xml_data(L210.GrdRenewRsrcMax_geo_CHINA, "GrdRenewRsrcMax") %>%
      add_xml_data(L210.SmthRenewRsrcCurvesGdpElast_roofPV_CHINA, "SmthRenewRsrcCurvesGdpElast") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_xml_data(L210.ResTechShrwt_CHINA, "ResTechShrwt") %>%
      add_precursors("L210.RenewRsrc_CHINA",
                     "L210.RenewRsrcPrice_CHINA",
                     "L210.UnlimitRsrc_CHINA",
                     "L210.UnlimitRsrcPrice_CHINA",
                     "L210.SmthRenewRsrcTechChange_CHINA",
                     "L210.SmthRenewRsrcCurves_wind_CHINA",
                     "L210.SmthRenewRsrcTechChange_offshore_wind_CHINA",
                     "L210.SmthRenewRsrcCurves_offshore_wind_CHINA",
                     "L210.GrdRenewRsrcCurves_geo_CHINA",
                     "L210.GrdRenewRsrcMax_geo_CHINA",
                     "L210.SmthRenewRsrcCurvesGdpElast_roofPV_CHINA",
                     "L210.ResTechShrwt_CHINA") ->
      resources_CHINA.xml

    return_data(resources_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
