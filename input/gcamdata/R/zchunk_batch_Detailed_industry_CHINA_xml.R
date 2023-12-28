# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_batch_detailed_industry_CHINA_xml
#'
#' Construct XML data structure for \code{detailed_industry_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{detailed_industry_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_detailed_industry_CHINA_xml.R} (gcamchina XML).
module_gcamchina_batch_detailed_industry_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/A323.UnlimitRsrc_CHINA",
             FILE = "gcam-china/A323.UnlimitRsrcPrice_CHINA",
             "L2323.Supplysector_detailed_industry",
             "L2323.FinalEnergyKeyword_detailed_industry",
             "L2323.SubsectorLogit_detailed_industry",
             "L2323.SubsectorShrwtFllt_detailed_industry",
             "L2323.SubsectorInterp_detailed_industry",
             "L2323.StubTech_detailed_industry",
             "L2323.GlobalTechShrwt_detailed_industry",
             "L2323.StubTechShrwt_detailed_industry",
             "L2323.GlobalTechCoef_detailed_industry",
             "L2323.GlobalTechCost_detailed_industry",
             "L2323.StubTechCost_detailed_industry",
             "L2323.GlobalTechCapture_detailed_industry",
             "L2323.StubTechProd_detailed_industry",
             "L2323.StubTechMarket_detailed_industry",
             "L2323.StubTechCoef_detailed_industry",
             "L2323.PerCapitaBased_detailed_industry",
             "L2323.BaseService_detailed_industry",
             "L2323.PriceElasticity_detailed_industry",
             "L2323.IncomeElasticity_detailed_industry",
             "L2323.Supplysector_detailed_industry_China",
             "L2323.SubsectorLogit_detailed_industry_China",
             "L2323.SubsectorInterp_detailed_industry_China",
             "L2323.TechCoef_detailed_industry_China",
             "L2323.TechShrwt_detailed_industry_China",
             "L2323.Production_detailed_industry_China",
             "L2323.GlobalTechSCurve_detailed_industry",
             "L2323.GlobalTechProfitShutdown_detailed_industry",
             "L2323.GlobalTechCSeq_ind"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "detailed_industry_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs

  L2323.Supplysector_detailed_industry <- get_data(all_data, "L2323.Supplysector_detailed_industry")
	L2323.FinalEnergyKeyword_detailed_industry <- get_data(all_data, "L2323.FinalEnergyKeyword_detailed_industry")
	L2323.SubsectorLogit_detailed_industry <- get_data(all_data, "L2323.SubsectorLogit_detailed_industry")
	L2323.SubsectorShrwtFllt_detailed_industry <- get_data(all_data, "L2323.SubsectorShrwtFllt_detailed_industry")
	L2323.SubsectorInterp_detailed_industry <- get_data(all_data, "L2323.SubsectorInterp_detailed_industry")
	L2323.StubTech_detailed_industry <- get_data(all_data, "L2323.StubTech_detailed_industry")
	L2323.GlobalTechShrwt_detailed_industry <- get_data(all_data, "L2323.GlobalTechShrwt_detailed_industry")
	L2323.StubTechShrwt_detailed_industry <- get_data(all_data, "L2323.StubTechShrwt_detailed_industry")
	L2323.GlobalTechCoef_detailed_industry <- get_data(all_data, "L2323.GlobalTechCoef_detailed_industry")
	L2323.GlobalTechCost_detailed_industry <- get_data(all_data, "L2323.GlobalTechCost_detailed_industry")
	L2323.StubTechCost_detailed_industry <- get_data(all_data, "L2323.StubTechCost_detailed_industry")
	L2323.GlobalTechCapture_detailed_industry <- get_data(all_data, "L2323.GlobalTechCapture_detailed_industry")
	L2323.StubTechProd_detailed_industry <- get_data(all_data, "L2323.StubTechProd_detailed_industry")
	L2323.StubTechMarket_detailed_industry <- get_data(all_data, "L2323.StubTechMarket_detailed_industry")
	L2323.StubTechCoef_detailed_industry <- get_data(all_data, "L2323.StubTechCoef_detailed_industry")
	L2323.PerCapitaBased_detailed_industry <- get_data(all_data, "L2323.PerCapitaBased_detailed_industry")
	L2323.BaseService_detailed_industry <- get_data(all_data, "L2323.BaseService_detailed_industry")
	L2323.PriceElasticity_detailed_industry <- get_data(all_data, "L2323.PriceElasticity_detailed_industry")
	L2323.IncomeElasticity_detailed_industry <- get_data(all_data, "L2323.IncomeElasticity_detailed_industry")
	L2323.Supplysector_detailed_industry_China <- get_data(all_data, "L2323.Supplysector_detailed_industry_China")
	L2323.SubsectorLogit_detailed_industry_China <- get_data(all_data, "L2323.SubsectorLogit_detailed_industry_China")
	L2323.SubsectorInterp_detailed_industry_China <- get_data(all_data, "L2323.SubsectorInterp_detailed_industry_China")
	L2323.TechCoef_detailed_industry_China <- get_data(all_data, "L2323.TechCoef_detailed_industry_China")
	L2323.TechShrwt_detailed_industry_China <- get_data(all_data, "L2323.TechShrwt_detailed_industry_China")
	L2323.Production_detailed_industry_China <- get_data(all_data, "L2323.Production_detailed_industry_China")
	L2323.GlobalTechSCurve_detailed_industry <- get_data(all_data, "L2323.GlobalTechSCurve_detailed_industry")
	L2323.GlobalTechProfitShutdown_detailed_industry <- get_data(all_data, "L2323.GlobalTechProfitShutdown_detailed_industry")
	L2323.GlobalTechCSeq_ind <- get_data(all_data, "L2323.GlobalTechCSeq_ind")

	A323.UnlimitRsrc_CHINA <- get_data(all_data, "gcam-china/A323.UnlimitRsrc_CHINA")
	A323.UnlimitRsrcPrice_CHINA <- get_data(all_data, "gcam-china/A323.UnlimitRsrcPrice_CHINA")


    # ===================================================
	if (gcamchina.USE_LOW_IND_DEMAND){
	  # Produce outputs
	  create_xml("detailed_industry_CHINA.xml") %>%
	    add_xml_data(A323.UnlimitRsrc_CHINA, "UnlimitRsrc") %>%
	    add_xml_data(A323.UnlimitRsrcPrice_CHINA, "UnlimitRsrcPrice") %>%
	    add_xml_data(L2323.FinalEnergyKeyword_detailed_industry, "FinalEnergyKeyword") %>%
	    add_xml_data(L2323.SubsectorShrwtFllt_detailed_industry, "SubsectorShrwtFllt") %>%
	    add_xml_data(L2323.SubsectorInterp_detailed_industry, "SubsectorInterp") %>%
	    add_xml_data(L2323.StubTech_detailed_industry, "StubTech") %>%
	    #add_xml_data(L2323.GlobalTechShrwt_detailed_industry, "GlobalTechShrwt") %>%
	    add_xml_data(L2323.StubTechShrwt_detailed_industry, "StubTechShrwt") %>%
	    #add_xml_data(L2323.GlobalTechCoef_detailed_industry, "GlobalTechCoef") %>%
	    #add_xml_data(L2323.GlobalTechCost_detailed_industry, "GlobalTechCost") %>%
	    add_xml_data(L2323.StubTechCost_detailed_industry, "StubTechCost") %>%
	    add_xml_data(L2323.GlobalTechCapture_detailed_industry, "GlobalTechCapture") %>%
	    add_xml_data(L2323.StubTechProd_detailed_industry, "StubTechProd") %>%
	    add_xml_data(L2323.StubTechMarket_detailed_industry, "StubTechMarket") %>%
	    add_xml_data(L2323.StubTechCoef_detailed_industry, "StubTechCoef") %>%
	    add_xml_data(L2323.PerCapitaBased_detailed_industry, "PerCapitaBased") %>%
	    add_xml_data(L2323.BaseService_detailed_industry, "BaseService") %>%
	    add_xml_data(L2323.PriceElasticity_detailed_industry, "PriceElasticity") %>%
	    add_xml_data(L2323.IncomeElasticity_detailed_industry, "IncomeElasticity") %>%
	    add_xml_data(L2323.SubsectorInterp_detailed_industry_China, "SubsectorInterp") %>%
	    add_xml_data(L2323.TechCoef_detailed_industry_China, "TechCoef") %>%
	    add_xml_data(L2323.TechShrwt_detailed_industry_China, "TechShrwt") %>%
	    add_xml_data(L2323.Production_detailed_industry_China, "Production") %>%
	    add_xml_data(L2323.GlobalTechSCurve_detailed_industry, "GlobalTechSCurve") %>%
	    add_xml_data(L2323.GlobalTechProfitShutdown_detailed_industry, "GlobalTechProfitShutdown") %>%
	    add_xml_data(L2323.GlobalTechCSeq_ind, "GlobalTechCSeq") %>%
	    add_logit_tables_xml(L2323.Supplysector_detailed_industry, "Supplysector") %>%
	    add_logit_tables_xml(L2323.SubsectorLogit_detailed_industry, "SubsectorLogit") %>%
	    add_logit_tables_xml(L2323.Supplysector_detailed_industry_China, "Supplysector") %>%
	    add_logit_tables_xml(L2323.SubsectorLogit_detailed_industry_China, "SubsectorLogit") %>%
	    add_precursors("L2323.Supplysector_detailed_industry",
	                   "L2323.FinalEnergyKeyword_detailed_industry",
	                   "L2323.SubsectorLogit_detailed_industry",
	                   "L2323.SubsectorShrwtFllt_detailed_industry",
	                   "L2323.SubsectorInterp_detailed_industry",
	                   "L2323.StubTech_detailed_industry",
	                   #"L2323.GlobalTechShrwt_detailed_industry",
	                   "L2323.StubTechShrwt_detailed_industry",
	                   #"L2323.GlobalTechCoef_detailed_industry",
	                   #"L2323.GlobalTechCost_detailed_industry",
	                   "L2323.StubTechCost_detailed_industry",
	                   "L2323.GlobalTechCapture_detailed_industry",
	                   "L2323.StubTechProd_detailed_industry",
	                   "L2323.StubTechMarket_detailed_industry",
	                   "L2323.StubTechCoef_detailed_industry",
	                   "L2323.PerCapitaBased_detailed_industry",
	                   "L2323.BaseService_detailed_industry",
	                   "L2323.PriceElasticity_detailed_industry",
	                   "L2323.IncomeElasticity_detailed_industry",
	                   "L2323.Supplysector_detailed_industry_China",
	                   "L2323.SubsectorLogit_detailed_industry_China",
	                   "L2323.SubsectorInterp_detailed_industry_China",
	                   "L2323.TechCoef_detailed_industry_China",
	                   "L2323.TechShrwt_detailed_industry_China",
	                   "L2323.Production_detailed_industry_China",
	                   "L2323.GlobalTechSCurve_detailed_industry",
	                   "L2323.GlobalTechProfitShutdown_detailed_industry",
	                   "L2323.GlobalTechCSeq_ind",
	                   "gcam-china/A323.UnlimitRsrc_CHINA",
	                   "gcam-china/A323.UnlimitRsrcPrice_CHINA") ->
	    detailed_industry_CHINA.xml
	}else{
	  # Produce outputs
	  #only use the value of 2020 to calibration
	  L2323.IncomeElasticity_detailed_industry %>%
	    filter(year %in%  c(2020)) ->
	    L2323.IncomeElasticity_detailed_industry

	  create_xml("detailed_industry_CHINA.xml") %>%
	    add_xml_data(A323.UnlimitRsrc_CHINA, "UnlimitRsrc") %>%
	    add_xml_data(A323.UnlimitRsrcPrice_CHINA, "UnlimitRsrcPrice") %>%
	    add_xml_data(L2323.FinalEnergyKeyword_detailed_industry, "FinalEnergyKeyword") %>%
	    add_xml_data(L2323.SubsectorShrwtFllt_detailed_industry, "SubsectorShrwtFllt") %>%
	    add_xml_data(L2323.SubsectorInterp_detailed_industry, "SubsectorInterp") %>%
	    add_xml_data(L2323.StubTech_detailed_industry, "StubTech") %>%
	    #add_xml_data(L2323.GlobalTechShrwt_detailed_industry, "GlobalTechShrwt") %>%
	    add_xml_data(L2323.StubTechShrwt_detailed_industry, "StubTechShrwt") %>%
	    #add_xml_data(L2323.GlobalTechCoef_detailed_industry, "GlobalTechCoef") %>%
	    #add_xml_data(L2323.GlobalTechCost_detailed_industry, "GlobalTechCost") %>%
	    add_xml_data(L2323.StubTechCost_detailed_industry, "StubTechCost") %>%
	    add_xml_data(L2323.GlobalTechCapture_detailed_industry, "GlobalTechCapture") %>%
	    add_xml_data(L2323.StubTechProd_detailed_industry, "StubTechProd") %>%
	    add_xml_data(L2323.StubTechMarket_detailed_industry, "StubTechMarket") %>%
	    add_xml_data(L2323.StubTechCoef_detailed_industry, "StubTechCoef") %>%
	    add_xml_data(L2323.PerCapitaBased_detailed_industry, "PerCapitaBased") %>%
	    add_xml_data(L2323.BaseService_detailed_industry, "BaseService") %>%
	    add_xml_data(L2323.PriceElasticity_detailed_industry, "PriceElasticity") %>%
	    add_xml_data(L2323.IncomeElasticity_detailed_industry, "IncomeElasticity") %>%
	    add_xml_data(L2323.SubsectorInterp_detailed_industry_China, "SubsectorInterp") %>%
	    add_xml_data(L2323.TechCoef_detailed_industry_China, "TechCoef") %>%
	    add_xml_data(L2323.TechShrwt_detailed_industry_China, "TechShrwt") %>%
	    add_xml_data(L2323.Production_detailed_industry_China, "Production") %>%
	    add_xml_data(L2323.GlobalTechSCurve_detailed_industry, "GlobalTechSCurve") %>%
	    add_xml_data(L2323.GlobalTechProfitShutdown_detailed_industry, "GlobalTechProfitShutdown") %>%
	    add_xml_data(L2323.GlobalTechCSeq_ind, "GlobalTechCSeq") %>%
	    add_logit_tables_xml(L2323.Supplysector_detailed_industry, "Supplysector") %>%
	    add_logit_tables_xml(L2323.SubsectorLogit_detailed_industry, "SubsectorLogit") %>%
	    add_logit_tables_xml(L2323.Supplysector_detailed_industry_China, "Supplysector") %>%
	    add_logit_tables_xml(L2323.SubsectorLogit_detailed_industry_China, "SubsectorLogit") %>%
	    add_precursors("L2323.Supplysector_detailed_industry",
	                   "L2323.FinalEnergyKeyword_detailed_industry",
	                   "L2323.SubsectorLogit_detailed_industry",
	                   "L2323.SubsectorShrwtFllt_detailed_industry",
	                   "L2323.SubsectorInterp_detailed_industry",
	                   "L2323.StubTech_detailed_industry",
	                   #"L2323.GlobalTechShrwt_detailed_industry",
	                   "L2323.StubTechShrwt_detailed_industry",
	                   #"L2323.GlobalTechCoef_detailed_industry",
	                   #"L2323.GlobalTechCost_detailed_industry",
	                   "L2323.StubTechCost_detailed_industry",
	                   "L2323.GlobalTechCapture_detailed_industry",
	                   "L2323.StubTechProd_detailed_industry",
	                   "L2323.StubTechMarket_detailed_industry",
	                   "L2323.StubTechCoef_detailed_industry",
	                   "L2323.PerCapitaBased_detailed_industry",
	                   "L2323.BaseService_detailed_industry",
	                   "L2323.PriceElasticity_detailed_industry",
	                   "L2323.IncomeElasticity_detailed_industry",
	                   "L2323.Supplysector_detailed_industry_China",
	                   "L2323.SubsectorLogit_detailed_industry_China",
	                   "L2323.SubsectorInterp_detailed_industry_China",
	                   "L2323.TechCoef_detailed_industry_China",
	                   "L2323.TechShrwt_detailed_industry_China",
	                   "L2323.Production_detailed_industry_China",
	                   "L2323.GlobalTechSCurve_detailed_industry",
	                   "L2323.GlobalTechProfitShutdown_detailed_industry",
	                   "L2323.GlobalTechCSeq_ind",
	                   "gcam-china/A323.UnlimitRsrc_CHINA",
	                   "gcam-china/A323.UnlimitRsrcPrice_CHINA") ->
	    detailed_industry_CHINA.xml
	}

    return_data(detailed_industry_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
