# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_batch_Fert_CHINA_xml
#'
#' Construct XML data structure for \code{Fert_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{Fert_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_Fert_CHINA_xml.R} (gcamchina XML).
module_gcamchina_batch_Fert_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2322.DeleteSubsector_CHINAFert",
             "L2322.FinalEnergyKeyword_Fert_CHINA",
             "L2322.Supplysector_Fert_CHINA",
             "L2322.SubsectorLogit_Fert_CHINA",
             "L2322.SubsectorShrwtFllt_Fert_CHINA",
             "L2322.SubsectorInterp_Fert_CHINA",
             "L2322.StubTech_Fert_CHINA",
             "L2322.FinalEnergyKeyword_CHINAFert",
             "L2322.SubsectorLogit_CHINAFert",
             "L2322.SubsectorShrwtFllt_CHINAFert",
             "L2322.SubsectorInterp_CHINAFert",
             "L2322.TechShrwt_CHINAFert",
             "L2322.Production_CHINAFert",
             "L2322.TechCoef_CHINAFert",
             "L2322.StubTechProd_Fert_CHINA",
             "L2322.StubTechCoef_Fert_CHINA",
             "L2322.StubTechMarket_Fert_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "Fert_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2322.DeleteSubsector_CHINAFert <- get_data(all_data, "L2322.DeleteSubsector_CHINAFert")
    L2322.FinalEnergyKeyword_Fert_CHINA <- get_data(all_data, "L2322.FinalEnergyKeyword_Fert_CHINA")
    L2322.Supplysector_Fert_CHINA <- get_data(all_data, "L2322.Supplysector_Fert_CHINA")
    L2322.SubsectorLogit_Fert_CHINA <- get_data(all_data, "L2322.SubsectorLogit_Fert_CHINA")
    L2322.SubsectorShrwtFllt_Fert_CHINA <- get_data(all_data, "L2322.SubsectorShrwtFllt_Fert_CHINA")
    L2322.SubsectorInterp_Fert_CHINA <- get_data(all_data, "L2322.SubsectorInterp_Fert_CHINA")
    L2322.StubTech_Fert_CHINA <- get_data(all_data, "L2322.StubTech_Fert_CHINA")
    L2322.FinalEnergyKeyword_CHINAFert <- get_data(all_data, "L2322.FinalEnergyKeyword_CHINAFert")
    L2322.SubsectorLogit_CHINAFert <- get_data(all_data, "L2322.SubsectorLogit_CHINAFert")
    L2322.SubsectorShrwtFllt_CHINAFert <- get_data(all_data, "L2322.SubsectorShrwtFllt_CHINAFert")
    L2322.SubsectorInterp_CHINAFert <- get_data(all_data, "L2322.SubsectorInterp_CHINAFert")
    L2322.TechShrwt_CHINAFert <- get_data(all_data, "L2322.TechShrwt_CHINAFert")
    L2322.Production_CHINAFert <- get_data(all_data, "L2322.Production_CHINAFert")
    L2322.TechCoef_CHINAFert <- get_data(all_data, "L2322.TechCoef_CHINAFert")
    L2322.StubTechProd_Fert_CHINA <- get_data(all_data, "L2322.StubTechProd_Fert_CHINA")
    L2322.StubTechCoef_Fert_CHINA <- get_data(all_data, "L2322.StubTechCoef_Fert_CHINA")
    L2322.StubTechMarket_Fert_CHINA <- get_data(all_data, "L2322.StubTechMarket_Fert_CHINA")

    # ===================================================

    # Produce outputs
    create_xml("Fert_CHINA.xml") %>%
      add_xml_data(L2322.DeleteSubsector_CHINAFert, "DeleteSubsector") %>%
      add_logit_tables_xml(L2322.Supplysector_Fert_CHINA, "Supplysector") %>%
      add_logit_tables_xml(L2322.SubsectorLogit_Fert_CHINA, "SubsectorLogit") %>%
      add_xml_data(L2322.FinalEnergyKeyword_Fert_CHINA, "FinalEnergyKeyword") %>%
      add_xml_data(L2322.SubsectorShrwtFllt_Fert_CHINA, "SubsectorShrwtFllt") %>%
      add_xml_data(L2322.SubsectorInterp_Fert_CHINA, "SubsectorInterp") %>%
      add_xml_data(L2322.StubTech_Fert_CHINA, "StubTech") %>%
      add_xml_data(L2322.FinalEnergyKeyword_CHINAFert, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L2322.SubsectorLogit_CHINAFert, "SubsectorLogit") %>%
      add_xml_data(L2322.SubsectorShrwtFllt_CHINAFert, "SubsectorShrwtFllt") %>%
      add_xml_data(L2322.SubsectorInterp_CHINAFert, "SubsectorInterp") %>%
      add_xml_data(L2322.TechShrwt_CHINAFert, "TechShrwt") %>%
      add_xml_data(L2322.Production_CHINAFert, "Production") %>%
      add_xml_data(L2322.TechCoef_CHINAFert, "TechCoef") %>%
      add_xml_data(L2322.StubTechProd_Fert_CHINA, "StubTechProd") %>%
      add_xml_data(L2322.StubTechCoef_Fert_CHINA, "StubTechCoef") %>%
      add_xml_data(L2322.StubTechMarket_Fert_CHINA, "StubTechMarket") %>%
      add_precursors("L2322.DeleteSubsector_CHINAFert",
                     "L2322.FinalEnergyKeyword_Fert_CHINA",
                     "L2322.Supplysector_Fert_CHINA",
                     "L2322.SubsectorLogit_Fert_CHINA",
                     "L2322.SubsectorShrwtFllt_Fert_CHINA",
                     "L2322.SubsectorInterp_Fert_CHINA",
                     "L2322.StubTech_Fert_CHINA",
                     "L2322.FinalEnergyKeyword_CHINAFert",
                     "L2322.SubsectorLogit_CHINAFert",
                     "L2322.SubsectorShrwtFllt_CHINAFert",
                     "L2322.SubsectorInterp_CHINAFert",
                     "L2322.TechShrwt_CHINAFert",
                     "L2322.Production_CHINAFert",
                     "L2322.TechCoef_CHINAFert",
                     "L2322.StubTechProd_Fert_CHINA",
                     "L2322.StubTechCoef_Fert_CHINA",
                     "L2322.StubTechMarket_Fert_CHINA") ->
      Fert_CHINA.xml

    return_data(Fert_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
