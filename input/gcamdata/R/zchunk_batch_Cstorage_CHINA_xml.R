# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_batch_Cstorage_CHINA_xml
#'
#' Construct XML data structure for \code{Cstorage_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{Cstorage_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_Cstorage_CHINA_xml.R} (gcamchina XML).
module_gcamchina_batch_Cstorage_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L261.DeleteSubsector_CHINAC",
             "L261.Rsrc_province",
             "L261.RsrcCurves_province",
             "L261.Supplysector_C_CHINA",
             "L261.SubsectorLogit_C_CHINA",
             "L261.SubsectorShrwtFllt_C_CHINA",
             "L261.StubTech_C_CHINA",
             "L261.StubTechMarket_C_CHINA",
             "L261.DeleteRsrc_CHINAC",
             "L261.UnlimitRsrc_CHINA",
             "L261.GlobalTechCost_C_CHINA",
             "L261.ResTechShrwt_C_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "Cstorage_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L261.DeleteSubsector_CHINAC <- get_data(all_data, "L261.DeleteSubsector_CHINAC")
    L261.Rsrc_province <- get_data(all_data, "L261.Rsrc_province")
    L261.RsrcCurves_province <- get_data(all_data, "L261.RsrcCurves_province")
    L261.Supplysector_C_CHINA <- get_data(all_data, "L261.Supplysector_C_CHINA")
    L261.SubsectorLogit_C_CHINA <- get_data(all_data, "L261.SubsectorLogit_C_CHINA")
    L261.SubsectorShrwtFllt_C_CHINA <- get_data(all_data, "L261.SubsectorShrwtFllt_C_CHINA")
    L261.StubTech_C_CHINA <- get_data(all_data, "L261.StubTech_C_CHINA")
    L261.StubTechMarket_C_CHINA <- get_data(all_data, "L261.StubTechMarket_C_CHINA")
    L261.DeleteRsrc_CHINAC <- get_data(all_data, "L261.DeleteRsrc_CHINAC")
    L261.UnlimitRsrc_CHINA <- get_data(all_data, "L261.UnlimitRsrc_CHINA")
    L261.GlobalTechCost_C_CHINA <- get_data(all_data, "L261.GlobalTechCost_C_CHINA")
    L261.ResTechShrwt_C_CHINA <- get_data(all_data, "L261.ResTechShrwt_C_CHINA")

    # ===================================================

    # Produce outputs
    create_xml("Cstorage_CHINA.xml") %>%
      add_xml_data(L261.DeleteRsrc_CHINAC, "DeleteRsrc") %>%
      add_xml_data(L261.DeleteSubsector_CHINAC, "DeleteSubsector") %>%
      add_xml_data(L261.Rsrc_province, "Rsrc") %>%
      add_xml_data(L261.RsrcCurves_province, "RsrcCurves") %>%
      add_logit_tables_xml(L261.Supplysector_C_CHINA, "Supplysector") %>%
      add_logit_tables_xml(L261.SubsectorLogit_C_CHINA, "SubsectorLogit") %>%
      add_xml_data(L261.SubsectorShrwtFllt_C_CHINA, "SubsectorShrwtFllt") %>%
      add_xml_data(L261.StubTech_C_CHINA, "StubTech") %>%
      add_xml_data(L261.StubTechMarket_C_CHINA, "StubTechMarket") %>%
      add_xml_data(L261.UnlimitRsrc_CHINA, "UnlimitRsrc") %>%
      add_xml_data(L261.GlobalTechCost_C_CHINA, "GlobalTechCost") %>%
      add_xml_data(L261.ResTechShrwt_C_CHINA, "ResTechShrwt") %>%
      add_precursors("L261.DeleteSubsector_CHINAC",
                     "L261.Rsrc_province",
                     "L261.RsrcCurves_province",
                     "L261.Supplysector_C_CHINA",
                     "L261.SubsectorLogit_C_CHINA",
                     "L261.SubsectorShrwtFllt_C_CHINA",
                     "L261.StubTech_C_CHINA",
                     "L261.StubTechMarket_C_CHINA",
                     "L261.DeleteRsrc_CHINAC",
                     "L261.UnlimitRsrc_CHINA",
                     "L261.GlobalTechCost_C_CHINA",
                     "L261.ResTechShrwt_C_CHINA") ->
      Cstorage_CHINA.xml

    return_data(Cstorage_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
