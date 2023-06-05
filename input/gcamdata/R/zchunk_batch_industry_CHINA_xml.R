# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_batch_industry_CHINA_xml
#'
#' Construct XML data structure for \code{industry_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{industry_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_industry_CHINA_xml.R} (gcamchina XML).
module_gcamchina_batch_industry_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L232.StubTechCalInput_indenergy_CHINA",
            "L232.StubTechCalInput_indfeed_CHINA",
            "L232.StubTechProd_industry_CHINA",
            "L232.StubTechCoef_industry_CHINA",
            "L232.StubTechMarket_ind_CHINA",
            "L232.StubTechSecMarket_ind_CHINA",
            "L232.BaseService_ind_CHINA",
            "L232.DeleteSubsector_ind_CHINA",
            "L232.Supplysector_ind_CHINA",
            "L232.FinalEnergyKeyword_ind_CHINA",
            "L232.SubsectorLogit_ind_CHINA",
            "L232.SubsectorShrwtFllt_ind_CHINA",
            "L232.SubsectorInterp_ind_CHINA",
            "L232.StubTech_ind_CHINA",
            "L232.StubTechInterp_ind_CHINA",
            "L232.PerCapitaBased_ind_CHINA",
            "L232.PriceElasticity_ind_CHINA",
            "L232.IncomeElasticity_ind_gcam3_CHINA",
            "L232.DeleteSupplysector_CHINAind",
            "L232.DeleteFinalDemand_CHINAind"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "industry_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L232.StubTechCalInput_indenergy_CHINA <- get_data(all_data, "L232.StubTechCalInput_indenergy_CHINA")
    L232.StubTechCalInput_indfeed_CHINA <- get_data(all_data, "L232.StubTechCalInput_indfeed_CHINA")
    L232.StubTechProd_industry_CHINA <- get_data(all_data, "L232.StubTechProd_industry_CHINA")
    L232.StubTechCoef_industry_CHINA <- get_data(all_data, "L232.StubTechCoef_industry_CHINA")
    L232.StubTechMarket_ind_CHINA <- get_data(all_data, "L232.StubTechMarket_ind_CHINA")
    L232.StubTechSecMarket_ind_CHINA <- get_data(all_data, "L232.StubTechSecMarket_ind_CHINA")
    L232.BaseService_ind_CHINA <- get_data(all_data, "L232.BaseService_ind_CHINA")
    L232.DeleteSubsector_ind_CHINA <- get_data(all_data, "L232.DeleteSubsector_ind_CHINA")
    L232.Supplysector_ind_CHINA <- get_data(all_data, "L232.Supplysector_ind_CHINA")
    L232.FinalEnergyKeyword_ind_CHINA <- get_data(all_data, "L232.FinalEnergyKeyword_ind_CHINA")
    L232.SubsectorLogit_ind_CHINA <- get_data(all_data, "L232.SubsectorLogit_ind_CHINA")
    L232.SubsectorShrwtFllt_ind_CHINA <- get_data(all_data, "L232.SubsectorShrwtFllt_ind_CHINA")
    L232.SubsectorInterp_ind_CHINA <- get_data(all_data, "L232.SubsectorInterp_ind_CHINA")
    L232.StubTech_ind_CHINA <- get_data(all_data, "L232.StubTech_ind_CHINA")
    L232.StubTechInterp_ind_CHINA <- get_data(all_data, "L232.StubTechInterp_ind_CHINA")
    L232.PerCapitaBased_ind_CHINA <- get_data(all_data, "L232.PerCapitaBased_ind_CHINA")
    L232.PriceElasticity_ind_CHINA <- get_data(all_data, "L232.PriceElasticity_ind_CHINA")
    L232.IncomeElasticity_ind_gcam3_CHINA <- get_data(all_data, "L232.IncomeElasticity_ind_gcam3_CHINA")
    L232.DeleteSupplysector_CHINAind <- get_data(all_data, "L232.DeleteSupplysector_CHINAind")
	  L232.DeleteFinalDemand_CHINAind <- get_data(all_data, "L232.DeleteFinalDemand_CHINAind")

    # ===================================================

    # Produce outputs
  create_xml("industry_CHINA.xml") %>%
	  add_xml_data(L232.DeleteSupplysector_CHINAind, "DeleteSupplysector") %>%
	  add_xml_data(L232.DeleteFinalDemand_CHINAind, "DeleteFinalDemand") %>%
	  add_logit_tables_xml(L232.Supplysector_ind_CHINA, "Supplysector") %>%
	  add_logit_tables_xml(L232.SubsectorLogit_ind_CHINA, "SubsectorLogit") %>%
	  add_xml_data(L232.FinalEnergyKeyword_ind_CHINA, "FinalEnergyKeyword") %>%
	  add_xml_data(L232.SubsectorShrwtFllt_ind_CHINA, "SubsectorShrwtFllt") %>%
	  add_xml_data(L232.SubsectorInterp_ind_CHINA, "SubsectorInterp") %>%
	  add_xml_data(L232.StubTech_ind_CHINA, "StubTech") %>%
	  add_xml_data(L232.StubTechInterp_ind_CHINA, "StubTechInterp") %>%
	  add_xml_data(L232.PerCapitaBased_ind_CHINA, "PerCapitaBased") %>%
	  add_xml_data(L232.PriceElasticity_ind_CHINA, "PriceElasticity") %>%
	  add_xml_data(L232.IncomeElasticity_ind_gcam3_CHINA, "IncomeElasticity") %>%
	  add_xml_data(L232.StubTechCalInput_indenergy_CHINA, "StubTechCalInput") %>%
	  add_xml_data(L232.StubTechCalInput_indfeed_CHINA, "StubTechCalInput") %>%
	  add_xml_data(L232.StubTechProd_industry_CHINA, "StubTechProd") %>%
	  add_xml_data(L232.StubTechCoef_industry_CHINA, "StubTechCoef") %>%
	  add_xml_data(L232.StubTechMarket_ind_CHINA, "StubTechMarket") %>%
	  add_xml_data(L232.StubTechSecMarket_ind_CHINA, "StubTechSecMarket") %>%
	  add_xml_data(L232.BaseService_ind_CHINA, "BaseService") %>%
	  add_xml_data(L232.DeleteSubsector_ind_CHINA, "DeleteSubsector") %>%
    add_precursors("L232.StubTechCalInput_indenergy_CHINA",
					"L232.StubTechCalInput_indfeed_CHINA",
					"L232.StubTechProd_industry_CHINA",
					"L232.StubTechCoef_industry_CHINA",
					"L232.StubTechMarket_ind_CHINA",
					"L232.StubTechSecMarket_ind_CHINA",
					"L232.BaseService_ind_CHINA",
					"L232.DeleteSubsector_ind_CHINA",
					"L232.Supplysector_ind_CHINA",
					"L232.FinalEnergyKeyword_ind_CHINA",
					"L232.SubsectorLogit_ind_CHINA",
					"L232.SubsectorShrwtFllt_ind_CHINA",
					"L232.SubsectorInterp_ind_CHINA",
					"L232.StubTech_ind_CHINA",
					"L232.StubTechInterp_ind_CHINA",
					"L232.PerCapitaBased_ind_CHINA",
					"L232.PriceElasticity_ind_CHINA",
					"L232.IncomeElasticity_ind_gcam3_CHINA",
					"L232.DeleteSupplysector_CHINAind",
					"L232.DeleteFinalDemand_CHINAind") ->
      industry_CHINA.xml

    return_data(industry_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
