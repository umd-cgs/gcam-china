#' module_gcamchina_batch_transportation_CHINA_xml
#'
#' Construct XML data structure for \code{transportation_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_CHINA_xml.R} (gcamchina XML).
module_gcamchina_batch_transportation_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.DeleteSupplysector_CHINAtrn",
             "L254.DeleteFinalDemand_CHINAtrn",
             "L254.Supplysector_trn_CHINA",
             "L254.FinalEnergyKeyword_trn_CHINA",
             "L254.tranSubsectorLogit_CHINA",
             "L254.tranSubsectorShrwtFllt_CHINA",
             "L254.tranSubsectorInterp_CHINA",
             "L254.tranSubsectorSpeed_CHINA",
             "L254.tranSubsectorSpeed_passthru_CHINA",
             "L254.tranSubsectorSpeed_noVOTT_CHINA",
             "L254.tranSubsectorSpeed_nonmotor_CHINA",
             "L254.tranSubsectorVOTT_CHINA",
             "L254.tranSubsectorFuelPref_CHINA",
             "L254.StubTranTech_CHINA",
             "L254.StubTranTech_passthru_CHINA",
             "L254.StubTranTech_nonmotor_CHINA",
             "L254.StubTranTechLoadFactor_CHINA",
             "L254.StubTranTechCost_CHINA",
             "L254.StubTranTechCoef_CHINA",
             "L254.PerCapitaBased_trn_CHINA",
             "L254.PriceElasticity_trn_CHINA",
             "L254.IncomeElasticity_trn_CHINA",
             "L254.StubTranTechCalInput_CHINA",
             "L254.StubTranTechProd_nonmotor_CHINA",
             "L254.StubTranTechCalInput_passthru_CHINA",
             "L254.BaseService_trn_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "transportation_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L254.DeleteSupplysector_CHINAtrn <- get_data(all_data, "L254.DeleteSupplysector_CHINAtrn")
    L254.DeleteFinalDemand_CHINAtrn <- get_data(all_data, "L254.DeleteFinalDemand_CHINAtrn")
    L254.Supplysector_trn_CHINA <- get_data(all_data, "L254.Supplysector_trn_CHINA")
    L254.FinalEnergyKeyword_trn_CHINA <- get_data(all_data, "L254.FinalEnergyKeyword_trn_CHINA")
    L254.tranSubsectorLogit_CHINA <- get_data(all_data, "L254.tranSubsectorLogit_CHINA")
    L254.tranSubsectorShrwtFllt_CHINA <- get_data(all_data, "L254.tranSubsectorShrwtFllt_CHINA")
    L254.tranSubsectorInterp_CHINA <- get_data(all_data, "L254.tranSubsectorInterp_CHINA")
    L254.tranSubsectorSpeed_CHINA <- get_data(all_data, "L254.tranSubsectorSpeed_CHINA")
    L254.tranSubsectorSpeed_passthru_CHINA <- get_data(all_data, "L254.tranSubsectorSpeed_passthru_CHINA")
    L254.tranSubsectorSpeed_noVOTT_CHINA <- get_data(all_data, "L254.tranSubsectorSpeed_noVOTT_CHINA")
    L254.tranSubsectorSpeed_nonmotor_CHINA <- get_data(all_data, "L254.tranSubsectorSpeed_nonmotor_CHINA")
    L254.tranSubsectorVOTT_CHINA <- get_data(all_data, "L254.tranSubsectorVOTT_CHINA")
    L254.tranSubsectorFuelPref_CHINA <- get_data(all_data, "L254.tranSubsectorFuelPref_CHINA")
    L254.StubTranTech_CHINA <- get_data(all_data, "L254.StubTranTech_CHINA")
    L254.StubTranTech_passthru_CHINA <- get_data(all_data, "L254.StubTranTech_passthru_CHINA")
    L254.StubTranTech_nonmotor_CHINA <- get_data(all_data, "L254.StubTranTech_nonmotor_CHINA")
    L254.StubTranTechLoadFactor_CHINA <- get_data(all_data, "L254.StubTranTechLoadFactor_CHINA")
    L254.StubTranTechCost_CHINA <- get_data(all_data, "L254.StubTranTechCost_CHINA")
    L254.StubTranTechCoef_CHINA <- get_data(all_data, "L254.StubTranTechCoef_CHINA")
    L254.PerCapitaBased_trn_CHINA <- get_data(all_data, "L254.PerCapitaBased_trn_CHINA")
    L254.PriceElasticity_trn_CHINA <- get_data(all_data, "L254.PriceElasticity_trn_CHINA")
    L254.IncomeElasticity_trn_CHINA <- get_data(all_data, "L254.IncomeElasticity_trn_CHINA")
    L254.StubTranTechCalInput_CHINA <- get_data(all_data, "L254.StubTranTechCalInput_CHINA")
    L254.StubTranTechProd_nonmotor_CHINA <- get_data(all_data, "L254.StubTranTechProd_nonmotor_CHINA")
    L254.StubTranTechCalInput_passthru_CHINA <- get_data(all_data, "L254.StubTranTechCalInput_passthru_CHINA")
    L254.BaseService_trn_CHINA <- get_data(all_data, "L254.BaseService_trn_CHINA")
    # ===================================================

    # Produce outputs
    create_xml("transportation_CHINA.xml") %>%
      add_xml_data(L254.DeleteSupplysector_CHINAtrn, "DeleteSupplysector") %>%
      add_xml_data(L254.DeleteFinalDemand_CHINAtrn, "DeleteFinalDemand") %>%
      add_logit_tables_xml(L254.Supplysector_trn_CHINA, "Supplysector") %>%
      add_xml_data(L254.FinalEnergyKeyword_trn_CHINA, "FinalEnergyKeyword") %>%
      add_logit_tables_xml(L254.tranSubsectorLogit_CHINA, "tranSubsectorLogit", "tranSubsector") %>%
      add_xml_data(L254.tranSubsectorShrwtFllt_CHINA, "tranSubsectorShrwtFllt") %>%
      add_xml_data(L254.tranSubsectorInterp_CHINA, "tranSubsectorInterp") %>%
      add_xml_data(L254.tranSubsectorSpeed_CHINA, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_passthru_CHINA, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_noVOTT_CHINA, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorSpeed_nonmotor_CHINA, "tranSubsectorSpeed") %>%
      add_xml_data(L254.tranSubsectorVOTT_CHINA, "tranSubsectorVOTT") %>%
      add_xml_data(L254.tranSubsectorFuelPref_CHINA, "tranSubsectorFuelPref") %>%
      add_xml_data(L254.StubTranTech_CHINA, "StubTranTech") %>%
      add_xml_data(L254.StubTranTech_passthru_CHINA, "StubTranTech") %>%
      add_xml_data(L254.StubTranTech_nonmotor_CHINA, "StubTranTech") %>%
      add_xml_data(L254.StubTranTechLoadFactor_CHINA, "StubTranTechLoadFactor") %>%
      add_xml_data(L254.StubTranTechCost_CHINA, "StubTranTechCost") %>%
      add_xml_data(L254.StubTranTechCoef_CHINA, "StubTranTechCoef") %>%
      add_xml_data(L254.PerCapitaBased_trn_CHINA, "PerCapitaBased") %>%
      add_xml_data(L254.PriceElasticity_trn_CHINA, "PriceElasticity") %>%
      add_xml_data(L254.IncomeElasticity_trn_CHINA, "IncomeElasticity") %>%
      add_xml_data(L254.StubTranTechCalInput_CHINA, "StubTranTechCalInput") %>%
      add_xml_data(L254.StubTranTechProd_nonmotor_CHINA, "StubTranTechProd") %>%
      add_xml_data(L254.StubTranTechCalInput_passthru_CHINA, "StubTranTechCalInput") %>%
      add_xml_data(L254.BaseService_trn_CHINA, "BaseService") %>%
      add_precursors("L254.DeleteSupplysector_CHINAtrn",
                     "L254.DeleteFinalDemand_CHINAtrn",
                     "L254.Supplysector_trn_CHINA",
                     "L254.FinalEnergyKeyword_trn_CHINA",
                     "L254.tranSubsectorLogit_CHINA",
                     "L254.tranSubsectorShrwtFllt_CHINA",
                     "L254.tranSubsectorInterp_CHINA",
                     "L254.tranSubsectorSpeed_CHINA",
                     "L254.tranSubsectorSpeed_passthru_CHINA",
                     "L254.tranSubsectorSpeed_noVOTT_CHINA",
                     "L254.tranSubsectorSpeed_nonmotor_CHINA",
                     "L254.tranSubsectorVOTT_CHINA",
                     "L254.tranSubsectorFuelPref_CHINA",
                     "L254.StubTranTech_CHINA",
                     "L254.StubTranTech_passthru_CHINA",
                     "L254.StubTranTech_nonmotor_CHINA",
                     "L254.StubTranTechLoadFactor_CHINA",
                     "L254.StubTranTechCost_CHINA",
                     "L254.StubTranTechCoef_CHINA",
                     "L254.PerCapitaBased_trn_CHINA",
                     "L254.PriceElasticity_trn_CHINA",
                     "L254.IncomeElasticity_trn_CHINA",
                     "L254.StubTranTechCalInput_CHINA",
                     "L254.StubTranTechProd_nonmotor_CHINA",
                     "L254.StubTranTechCalInput_passthru_CHINA",
                     "L254.BaseService_trn_CHINA") ->
      transportation_CHINA.xml

    return_data(transportation_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
