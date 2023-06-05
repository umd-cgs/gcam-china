# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_batch_en_transformation_CHINA_xml
#'
#' Construct XML data structure for \code{en_transformation_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_transformation_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_en_transformation_CHINA_xml.R} (gcamchina XML).
module_gcamchina_batch_en_transformation_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L222.DeleteStubTech_CHINAen",
             "L222.PassThroughSector_CHINAen",
             "L222.SubsectorLogit_en_CHINA",
             "L222.StubTech_en_CHINA",
             "L222.StubTechCoef_refining_CHINA",
             "L222.GlobalTechInterp_en_CHINA",
             "L222.GlobalTechCoef_en_CHINA",
             "L222.GlobalTechCost_en_CHINA",
             "L222.GlobalTechShrwt_en_CHINA",
             "L222.GlobalTechCapture_en_CHINA",
             "L222.GlobalTechSCurve_en_CHINA",
             "L222.Tech_CHINAen",
             "L222.TechShrwt_CHINAen",
             "L222.TechInterp_CHINAen",
             "L222.TechCoef_CHINAen",
             "L222.Production_CHINArefining",
             "L222.Supplysector_en_CHINA",
             "L222.SubsectorShrwtFllt_en_CHINA",
             "L222.StubTechProd_refining_CHINA",
             "L222.StubTechMarket_en_CHINA",
             "L222.CarbonCoef_en_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_transformation_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L222.DeleteStubTech_CHINAen <- get_data(all_data, "L222.DeleteStubTech_CHINAen")
    L222.PassThroughSector_CHINAen <- get_data(all_data, "L222.PassThroughSector_CHINAen")
    L222.SubsectorLogit_en_CHINA <- get_data(all_data, "L222.SubsectorLogit_en_CHINA")
    L222.StubTech_en_CHINA <- get_data(all_data, "L222.StubTech_en_CHINA")
    L222.StubTechCoef_refining_CHINA <- get_data(all_data, "L222.StubTechCoef_refining_CHINA")
    L222.GlobalTechInterp_en_CHINA <- get_data(all_data, "L222.GlobalTechInterp_en_CHINA")
    L222.GlobalTechCoef_en_CHINA <- get_data(all_data, "L222.GlobalTechCoef_en_CHINA")
    L222.GlobalTechCost_en_CHINA <- get_data(all_data, "L222.GlobalTechCost_en_CHINA")
    L222.GlobalTechShrwt_en_CHINA <- get_data(all_data, "L222.GlobalTechShrwt_en_CHINA")
    L222.GlobalTechCapture_en_CHINA <- get_data(all_data, "L222.GlobalTechCapture_en_CHINA")
    L222.GlobalTechSCurve_en_CHINA <- get_data(all_data, "L222.GlobalTechSCurve_en_CHINA")
    L222.Tech_CHINAen <- get_data(all_data, "L222.Tech_CHINAen")
    L222.TechShrwt_CHINAen <- get_data(all_data, "L222.TechShrwt_CHINAen")
    L222.TechInterp_CHINAen <- get_data(all_data, "L222.TechInterp_CHINAen")
    L222.TechCoef_CHINAen <- get_data(all_data, "L222.TechCoef_CHINAen")
    L222.Production_CHINArefining <- get_data(all_data, "L222.Production_CHINArefining")
    L222.Supplysector_en_CHINA <- get_data(all_data, "L222.Supplysector_en_CHINA")
    L222.SubsectorShrwtFllt_en_CHINA <- get_data(all_data, "L222.SubsectorShrwtFllt_en_CHINA")
    L222.StubTechProd_refining_CHINA <- get_data(all_data, "L222.StubTechProd_refining_CHINA")
    L222.StubTechMarket_en_CHINA <- get_data(all_data, "L222.StubTechMarket_en_CHINA")
    L222.CarbonCoef_en_CHINA <- get_data(all_data, "L222.CarbonCoef_en_CHINA")

    technology <- year <- NULL # Silence package checks
    # ===================================================
    # Rename tibble columns to match the header information.
    L222.Tech_CHINAen <- rename(L222.Tech_CHINAen, pass.through.technology = technology)
    L222.SubsectorShrwtFllt_en_CHINA <- rename(L222.SubsectorShrwtFllt_en_CHINA, year.fillout = year)

    # Produce outputs
    create_xml("en_transformation_CHINA.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L222.DeleteStubTech_CHINAen, "DeleteStubTech") %>%
      add_xml_data(L222.PassThroughSector_CHINAen, "PassThroughSector") %>%
      add_logit_tables_xml(L222.SubsectorLogit_en_CHINA, "SubsectorLogit") %>%
      add_xml_data(L222.StubTech_en_CHINA, "StubTech") %>%
      add_xml_data(L222.StubTechCoef_refining_CHINA, "StubTechCoef") %>%
      add_xml_data(L222.GlobalTechInterp_en_CHINA, "GlobalTechInterp") %>%
      add_xml_data(L222.GlobalTechCoef_en_CHINA, "GlobalTechCoef") %>%
      add_xml_data(L222.GlobalTechCost_en_CHINA, "GlobalTechCost") %>%
      add_xml_data(L222.GlobalTechShrwt_en_CHINA, "GlobalTechShrwt") %>%
      add_xml_data(L222.GlobalTechCapture_en_CHINA, "GlobalTechCapture") %>%
      add_xml_data(L222.GlobalTechSCurve_en_CHINA, "GlobalTechSCurve") %>%
      add_xml_data(L222.Tech_CHINAen, "PassThroughTech") %>%
      add_xml_data(L222.TechInterp_CHINAen, "TechInterp") %>%
      add_xml_data(L222.TechShrwt_CHINAen, "TechShrwt") %>%
      add_xml_data(L222.TechShrwt_CHINAen, "TechShrwt") %>%
      add_xml_data(L222.TechCoef_CHINAen, "TechCoef") %>%
      add_xml_data(L222.Production_CHINArefining, "Production") %>%
      add_logit_tables_xml(L222.Supplysector_en_CHINA, "Supplysector") %>%
      add_xml_data(L222.SubsectorShrwtFllt_en_CHINA, "SubsectorShrwtFllt") %>%
      add_xml_data(L222.StubTechProd_refining_CHINA, "StubTechProd") %>%
      add_xml_data(L222.StubTechMarket_en_CHINA, "StubTechMarket") %>%
      add_xml_data(L222.CarbonCoef_en_CHINA, "CarbonCoef") %>%
      add_precursors("L222.DeleteStubTech_CHINAen",
                     "L222.PassThroughSector_CHINAen",
                     "L222.SubsectorLogit_en_CHINA",
                     "L222.StubTech_en_CHINA",
                     "L222.StubTechCoef_refining_CHINA",
                     "L222.GlobalTechInterp_en_CHINA",
                     "L222.GlobalTechCoef_en_CHINA",
                     "L222.GlobalTechCost_en_CHINA",
                     "L222.GlobalTechShrwt_en_CHINA",
                     "L222.GlobalTechCapture_en_CHINA",
                     "L222.GlobalTechSCurve_en_CHINA",
                     "L222.Tech_CHINAen",
                     "L222.TechShrwt_CHINAen",
                     "L222.TechInterp_CHINAen",
                     "L222.TechShrwt_CHINAen",
                     "L222.TechCoef_CHINAen",
                     "L222.Production_CHINArefining",
                     "L222.Supplysector_en_CHINA",
                     "L222.SubsectorShrwtFllt_en_CHINA",
                     "L222.StubTechProd_refining_CHINA",
                     "L222.StubTechMarket_en_CHINA",
                     "L222.CarbonCoef_en_CHINA") ->
      en_transformation_CHINA.xml

    return_data(en_transformation_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
