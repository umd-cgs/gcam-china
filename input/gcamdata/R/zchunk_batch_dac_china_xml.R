# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_batch_dac_china_xml
#'
#' Construct XML data structure for \code{dac_china.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{dac_china.xml}. The corresponding file in the
#' original data system was \code{batch_dac_china_xml.R} (gcamchina XML).
#' @author JF March 2021
module_gcamchina_batch_dac_china_xml <- function(command, ...) {

  TECH_PARAMETRIZATION_INPUTS <- paste0("ssp", 1:5)
  if(command == driver.DECLARE_INPUTS) {
    return(c("L262.DeleteSupplysector_chinadac",
             "L262.Supplysector_dac_china",
             "L262.FinalEnergyKeyword_dac_china",
             "L262.SubsectorLogit_dac_china",
             "L262.SubsectorShrwtFllt_dac_china",
             "L262.SubsectorInterp_dac_china",
             "L262.StubTech_dac_china",
             "L262.PerCapitaBased_dac_china",
             "L262.PriceElasticity_dac_china",
             "L262.DeleteFinalDemand_chinadac",
             "L262.StubTechProd_dac_china",
             "L262.StubTechCoef_dac_china_ssp1",
             "L262.StubTechCoef_dac_china_ssp2",
             "L262.StubTechCoef_dac_china_ssp3",
             "L262.StubTechCoef_dac_china_ssp4",
             "L262.StubTechCoef_dac_china_ssp5",
             "L262.BaseService_dac_china",
             "L262.CarbonCoef_dac_china",
             "L262.StubTechShrwt_dac_china_ssp1",
             "L262.StubTechShrwt_dac_china_ssp2",
             "L262.StubTechShrwt_dac_china_ssp3",
             "L262.StubTechShrwt_dac_china_ssp4",
             "L262.StubTechShrwt_dac_china_ssp5"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "dac_china_ssp1.xml",
             XML = "dac_china_ssp2.xml",
             XML = "dac_china_ssp3.xml",
             XML = "dac_china_ssp4.xml",
             XML = "dac_china_ssp5.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L262.DeleteSupplysector_chinadac <- get_data(all_data, "L262.DeleteSupplysector_chinadac")
    L262.FinalEnergyKeyword_dac_china <- get_data(all_data, "L262.FinalEnergyKeyword_dac_china")
    L262.SubsectorLogit_dac_china <- get_data(all_data, "L262.SubsectorLogit_dac_china")
    L262.SubsectorShrwtFllt_dac_china <- get_data(all_data, "L262.SubsectorShrwtFllt_dac_china")
    L262.SubsectorInterp_dac_china <- get_data(all_data, "L262.SubsectorInterp_dac_china")
    L262.StubTech_dac_china <- get_data(all_data, "L262.StubTech_dac_china")
    L262.PerCapitaBased_dac_china <- get_data(all_data, "L262.PerCapitaBased_dac_china")
    L262.PriceElasticity_dac_china <- get_data(all_data, "L262.PriceElasticity_dac_china")
    L262.DeleteFinalDemand_chinadac <- get_data(all_data, "L262.DeleteFinalDemand_chinadac")
    L262.Supplysector_dac_china <- get_data(all_data, "L262.Supplysector_dac_china")
    L262.StubTechProd_dac_china <- get_data(all_data, "L262.StubTechProd_dac_china")
    L262.BaseService_dac_china <- get_data(all_data, "L262.BaseService_dac_china")
    L262.CarbonCoef_dac_china <- get_data(all_data, "L262.CarbonCoef_dac_china")


    for(sce in TECH_PARAMETRIZATION_INPUTS){

      coef_name <- paste0("L262.StubTechCoef_dac_china_",tolower(sce))
      L262.StubTechCoef_dac_china <- get_data(all_data, coef_name)%>% filter(!(grepl("water", minicam.energy.input)))
	

      coef_name <- paste0("L262.StubTechShrwt_dac_china_",tolower(sce))
      L262.StubTechShrwt_dac_china <- get_data(all_data, coef_name)
      # ===================================================

      # Produce outputs
      xmlfn <- paste0("dac_china_",tolower(sce), '.xml')

      create_xml(xmlfn) %>%
        add_xml_data(L262.DeleteSupplysector_chinadac, "DeleteSupplysector") %>%
        add_xml_data(L262.DeleteFinalDemand_chinadac, "DeleteFinalDemand") %>%
        add_logit_tables_xml(L262.Supplysector_dac_china, "Supplysector") %>%
        add_xml_data(L262.FinalEnergyKeyword_dac_china, "FinalEnergyKeyword") %>%
        add_logit_tables_xml(L262.SubsectorLogit_dac_china, "SubsectorLogit") %>%
        add_xml_data(L262.SubsectorShrwtFllt_dac_china, "SubsectorShrwtFllt") %>%
        add_xml_data(L262.SubsectorInterp_dac_china, "SubsectorInterp") %>%
        add_xml_data(L262.StubTech_dac_china, "StubTech") %>%
        add_xml_data(L262.StubTechShrwt_dac_china, "StubTechShrwt") %>%
        add_xml_data(L262.PerCapitaBased_dac_china, "PerCapitaBased") %>%
        add_xml_data(L262.PriceElasticity_dac_china, "PriceElasticity") %>%
        add_xml_data(L262.StubTechProd_dac_china, "StubTechProd") %>%
        add_xml_data(L262.StubTechCoef_dac_china, "StubTechCoef") %>%
        add_xml_data(L262.BaseService_dac_china, "BaseService") %>%
        add_xml_data(L262.CarbonCoef_dac_china, "CarbonCoef") %>%
        add_precursors("L262.DeleteSupplysector_chinadac",
                       "L262.Supplysector_dac_china",
                       "L262.FinalEnergyKeyword_dac_china",
                       "L262.SubsectorLogit_dac_china",
                       "L262.SubsectorShrwtFllt_dac_china",
                       "L262.SubsectorInterp_dac_china",
                       "L262.StubTech_dac_china",
                       "L262.PerCapitaBased_dac_china",
                       "L262.PriceElasticity_dac_china",
                       "L262.DeleteFinalDemand_chinadac",
                       "L262.StubTechProd_dac_china",
                       paste0("L262.StubTechCoef_dac_china_",tolower(sce)),
                       "L262.BaseService_dac_china",
                       "L262.CarbonCoef_dac_china") ->
        xmlobj
      assign(xmlfn, xmlobj)
    }

    return_data(dac_china_ssp1.xml,
                dac_china_ssp2.xml,
                dac_china_ssp3.xml,
                dac_china_ssp4.xml,
                dac_china_ssp5.xml)
  } else {
    stop("Unknown command")
  }
}
