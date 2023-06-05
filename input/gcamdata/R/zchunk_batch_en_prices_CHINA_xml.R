#' module_gcamchina_batch_en_prices_CHINA_xml
#'
#' Construct XML data structure for \code{en_prices_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_prices_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_en_prices_CHINA_xml.R} (gcamCHINA XML).
module_gcamchina_batch_en_prices_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L226.Supplysector_en_CHINA",
             "L226.SubsectorShrwtFllt_en_CHINA",
             "L226.SubsectorLogit_en_CHINA",
             "L226.TechShrwt_en_CHINA",
             "L226.TechCoef_en_CHINA",
             "L226.TechCost_en_CHINA",
             "L226.Ccoef_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_prices_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    compVal <- passthrough.sector <- share.weight <-
      supplysector <- technology <- NULL # silence package check notes

    # Load required inputs
    L226.Supplysector_en_CHINA <- get_data(all_data, "L226.Supplysector_en_CHINA")
    L226.SubsectorShrwtFllt_en_CHINA <- get_data(all_data, "L226.SubsectorShrwtFllt_en_CHINA")
    L226.SubsectorLogit_en_CHINA <- get_data(all_data, "L226.SubsectorLogit_en_CHINA")
    L226.TechShrwt_en_CHINA <- get_data(all_data, "L226.TechShrwt_en_CHINA")
    L226.TechCoef_en_CHINA <- get_data(all_data, "L226.TechCoef_en_CHINA")
    L226.TechCost_en_CHINA <- get_data(all_data, "L226.TechCost_en_CHINA")
    L226.Ccoef_CHINA <- get_data(all_data, "L226.Ccoef_CHINA")

    # ===================================================
    # Rename tibble columns to match the L2 data header information.
    L226.Ccoef_CHINA <- rename(L226.Ccoef_CHINA, PrimaryFuelCO2Coef.name = supplysector)

    # Produce outputs
    create_xml("en_prices_CHINA.xml") %>%
      add_logit_tables_xml(L226.Supplysector_en_CHINA, "Supplysector") %>%
      add_xml_data(L226.SubsectorShrwtFllt_en_CHINA, "SubsectorShrwtFllt") %>%
      add_logit_tables_xml(L226.SubsectorLogit_en_CHINA, "SubsectorLogit") %>%
      add_xml_data(L226.TechShrwt_en_CHINA, "TechShrwt") %>%
      add_xml_data(L226.TechCoef_en_CHINA, "TechCoef") %>%
      add_xml_data(L226.TechCost_en_CHINA, "TechCost") %>%
      add_xml_data(L226.Ccoef_CHINA, "CarbonCoef") %>%
      add_precursors("L226.Supplysector_en_CHINA",
                     "L226.SubsectorShrwtFllt_en_CHINA",
                     "L226.SubsectorLogit_en_CHINA",
                     "L226.TechShrwt_en_CHINA",
                     "L226.TechCoef_en_CHINA",
                     "L226.TechCost_en_CHINA",
                     "L226.Ccoef_CHINA") ->
      en_prices_CHINA.xml

    return_data(en_prices_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
