# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_batch_regional_biomass_CHINA_xml
#'
#' Construct XML data structure for \code{regional_biomass_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{regional_biomass_CHINA.xml}.
#' The corresponding file in the original data system was \code{batch_regional_biomass_CHINA.xml} (gcamchina XML batch).
module_gcamchina_batch_regional_biomass_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2261.DeleteSupplysector_bio_CHINA",
             "L2261.Supplysector_bio_CHINA",
             "L2261.SubsectorShrwtFllt_bio_CHINA",
             "L2261.SubsectorInterp_bio_CHINA",
             "L2261.SubsectorLogit_bio_CHINA",
             "L2261.StubTech_bio_CHINA",
             "L2261.StubTechMarket_bio_CHINA",
             "L2261.StubTechCoef_bioOil_CHINA",
             "L2261.StubTechShrwt_rbO_CHINA",
             "L2261.StubTechFractSecOut_bio_CHINA",
             "L2261.StubTechFractProd_bio_CHINA",
             "L2261.StubTechFractCalPrice_bio_CHINA",
             "L2261.StubTechCalInput_bio_CHINA",
             "L2261.StubTechInterp_bio_CHINA",
             "L2261.Rsrc_DDGS_CHINA",
             "L2261.RsrcPrice_DDGS_CHINA",
             "L2261.Tech_rbm_CHINA",
             "L2261.TechShrwt_rbm_CHINA",
             "L2261.TechCoef_rbm_CHINA",
             "L2261.Tech_dbm_CHINA",
             "L2261.TechShrwt_dbm_CHINA",
             "L2261.TechEff_dbm_CHINA",
             "L2261.TechCost_dbm_CHINA",
             "L2261.CarbonCoef_bio_CHINA",
             "L2261.StubTechMarket_en_CHINA",
             "L2261.StubTechMarket_elecS_CHINA",
             "L2261.StubTechMarket_ind_CHINA",
             "L2261.StubTechMarket_cement_CHINA",
             "L2261.StubTechMarket_bld_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "regional_biomass_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    technology <- stub.technology <- NULL  # silence package check notes

    # Load required inputs
    L2261.DeleteSupplysector_bio_CHINA <- get_data(all_data, "L2261.DeleteSupplysector_bio_CHINA")
    L2261.Supplysector_bio_CHINA <- get_data(all_data, "L2261.Supplysector_bio_CHINA")
    L2261.SubsectorShrwtFllt_bio_CHINA <- get_data(all_data, "L2261.SubsectorShrwtFllt_bio_CHINA")
    L2261.SubsectorInterp_bio_CHINA <- get_data(all_data, "L2261.SubsectorInterp_bio_CHINA")
    L2261.SubsectorLogit_bio_CHINA <- get_data(all_data, "L2261.SubsectorLogit_bio_CHINA")
    L2261.StubTech_bio_CHINA <- get_data(all_data, "L2261.StubTech_bio_CHINA")
    L2261.StubTechMarket_bio_CHINA <- get_data(all_data, "L2261.StubTechMarket_bio_CHINA")
    L2261.StubTechCoef_bioOil_CHINA   <- get_data(all_data,"L2261.StubTechCoef_bioOil_CHINA")
    L2261.StubTechShrwt_rbO_CHINA <- get_data(all_data, "L2261.StubTechShrwt_rbO_CHINA")
    L2261.StubTechFractSecOut_bio_CHINA <- get_data(all_data, "L2261.StubTechFractSecOut_bio_CHINA")
    L2261.StubTechFractProd_bio_CHINA <- get_data(all_data, "L2261.StubTechFractProd_bio_CHINA")
    L2261.StubTechFractCalPrice_bio_CHINA <- get_data(all_data, "L2261.StubTechFractCalPrice_bio_CHINA")
    L2261.StubTechCalInput_bio_CHINA <- get_data(all_data, "L2261.StubTechCalInput_bio_CHINA")
    L2261.StubTechInterp_bio_CHINA <- get_data(all_data, "L2261.StubTechInterp_bio_CHINA")
    L2261.Rsrc_DDGS_CHINA <- get_data(all_data, "L2261.Rsrc_DDGS_CHINA")
    L2261.RsrcPrice_DDGS_CHINA <- get_data(all_data, "L2261.RsrcPrice_DDGS_CHINA")
    L2261.Tech_rbm_CHINA <- get_data(all_data, "L2261.Tech_rbm_CHINA")
    L2261.TechShrwt_rbm_CHINA <- get_data(all_data, "L2261.TechShrwt_rbm_CHINA")
    L2261.TechCoef_rbm_CHINA <- get_data(all_data, "L2261.TechCoef_rbm_CHINA")
    L2261.Tech_dbm_CHINA <- get_data(all_data, "L2261.Tech_dbm_CHINA")
    L2261.TechShrwt_dbm_CHINA <- get_data(all_data, "L2261.TechShrwt_dbm_CHINA")
    L2261.TechEff_dbm_CHINA <- get_data(all_data, "L2261.TechEff_dbm_CHINA")
    L2261.TechCost_dbm_CHINA <- get_data(all_data, "L2261.TechCost_dbm_CHINA")
    L2261.CarbonCoef_bio_CHINA <- get_data(all_data, "L2261.CarbonCoef_bio_CHINA")
    L2261.StubTechMarket_en_CHINA <- get_data(all_data, "L2261.StubTechMarket_en_CHINA")
    L2261.StubTechMarket_elecS_CHINA <- get_data(all_data, "L2261.StubTechMarket_elecS_CHINA")
    L2261.StubTechMarket_ind_CHINA <- get_data(all_data, "L2261.StubTechMarket_ind_CHINA")
    L2261.StubTechMarket_cement_CHINA <- get_data(all_data, "L2261.StubTechMarket_cement_CHINA")
    L2261.StubTechMarket_bld_CHINA <- get_data(all_data, "L2261.StubTechMarket_bld_CHINA")

    # ===================================================

    # Produce outputs
    create_xml("regional_biomass_CHINA.xml") %>%
      add_xml_data(L2261.DeleteSupplysector_bio_CHINA, "DeleteSupplysector") %>%
      add_node_equiv_xml("sector") %>%
      add_logit_tables_xml(L2261.Supplysector_bio_CHINA, "Supplysector") %>%
      add_xml_data(L2261.SubsectorShrwtFllt_bio_CHINA, "SubsectorShrwtFllt") %>%
      add_xml_data(L2261.SubsectorInterp_bio_CHINA, "SubsectorInterp") %>%
      add_logit_tables_xml(L2261.SubsectorLogit_bio_CHINA, "SubsectorLogit") %>%
      add_xml_data(L2261.StubTech_bio_CHINA, "StubTech") %>%
      add_xml_data(L2261.StubTechMarket_bio_CHINA, "StubTechMarket") %>%
      add_xml_data(L2261.StubTechCoef_bioOil_CHINA,"StubTechCoef") %>%
      add_xml_data(L2261.StubTechShrwt_rbO_CHINA, "StubTechShrwt") %>%
      add_xml_data(L2261.StubTechFractSecOut_bio_CHINA, "StubTechFractSecOut") %>%
      add_xml_data(L2261.StubTechFractProd_bio_CHINA, "StubTechFractProd") %>%
      add_xml_data(L2261.StubTechFractCalPrice_bio_CHINA, "StubTechFractCalPrice") %>%
      add_xml_data(L2261.StubTechCalInput_bio_CHINA, "StubTechCalInput") %>%
      add_xml_data(L2261.StubTechInterp_bio_CHINA, "StubTechInterp") %>%
      add_xml_data(L2261.Rsrc_DDGS_CHINA, "Rsrc") %>%
      add_xml_data(L2261.RsrcPrice_DDGS_CHINA, "RsrcPrice") %>%
      add_xml_data(L2261.Tech_rbm_CHINA, "Tech") %>%
      add_xml_data(L2261.TechShrwt_rbm_CHINA, "TechShrwt") %>%
      add_xml_data(L2261.TechCoef_rbm_CHINA, "TechCoef") %>%
      add_xml_data(L2261.Tech_dbm_CHINA, "Tech") %>%
      add_xml_data(L2261.TechShrwt_dbm_CHINA, "TechShrwt") %>%
      add_xml_data(L2261.TechEff_dbm_CHINA, "TechEff") %>%
      add_xml_data(L2261.TechCost_dbm_CHINA, "TechCost") %>%
      add_xml_data(L2261.CarbonCoef_bio_CHINA, "CarbonCoef") %>%
      add_xml_data(L2261.StubTechMarket_en_CHINA, "StubTechMarket") %>%
      add_xml_data_generate_levels(L2261.StubTechMarket_elecS_CHINA %>%
                                     rename(stub.technology = technology),
                                   "StubTechMarket","subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data(L2261.StubTechMarket_ind_CHINA, "StubTechMarket") %>%
      add_xml_data(L2261.StubTechMarket_cement_CHINA, "StubTechMarket") %>%
      add_xml_data(L2261.StubTechMarket_bld_CHINA, "StubTechMarket") %>%
      add_precursors("L2261.DeleteSupplysector_bio_CHINA",
                     "L2261.Supplysector_bio_CHINA",
                     "L2261.SubsectorShrwtFllt_bio_CHINA",
                     "L2261.SubsectorInterp_bio_CHINA",
                     "L2261.SubsectorLogit_bio_CHINA",
                     "L2261.StubTech_bio_CHINA",
                     "L2261.StubTechMarket_bio_CHINA",
                     "L2261.StubTechCoef_bioOil_CHINA",
                     "L2261.StubTechShrwt_rbO_CHINA",
                     "L2261.StubTechFractSecOut_bio_CHINA",
                     "L2261.StubTechFractProd_bio_CHINA",
                     "L2261.StubTechFractCalPrice_bio_CHINA",
                     "L2261.StubTechCalInput_bio_CHINA",
                     "L2261.StubTechInterp_bio_CHINA",
                     "L2261.Rsrc_DDGS_CHINA",
                     "L2261.RsrcPrice_DDGS_CHINA",
                     "L2261.Tech_rbm_CHINA",
                     "L2261.TechShrwt_rbm_CHINA",
                     "L2261.TechCoef_rbm_CHINA",
                     "L2261.Tech_dbm_CHINA",
                     "L2261.TechShrwt_dbm_CHINA",
                     "L2261.TechEff_dbm_CHINA",
                     "L2261.TechCost_dbm_CHINA",
                     "L2261.CarbonCoef_bio_CHINA",
                     "L2261.StubTechMarket_en_CHINA",
                     "L2261.StubTechMarket_elecS_CHINA",
                     "L2261.StubTechMarket_ind_CHINA",
                     "L2261.StubTechMarket_cement_CHINA",
                     "L2261.StubTechMarket_bld_CHINA") ->
      regional_biomass_CHINA.xml

    return_data(regional_biomass_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
