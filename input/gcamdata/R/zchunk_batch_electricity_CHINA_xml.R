# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_batch_electricity_CHINA_xml
#'
#' Construct XML data structure for \code{electricity_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{electricity_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_electricity_CHINA_xml.R} (gcamchina XML).
module_gcamchina_batch_electricity_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L223.Supplysector_elec_GRIDR",
             "L223.SubsectorShrwtFllt_elec_GRIDR",
             "L223.SubsectorInterp_elec_GRIDR",
             "L223.SubsectorLogit_elec_GRIDR",
             "L223.TechShrwt_elec_GRIDR",
             "L223.TechCoef_elec_GRIDR",
             "L223.Production_elec_GRIDR",
             "L223.InterestRate_GRIDR",
             "L223.Pop_GRIDR",
             "L223.BaseGDP_GRIDR",
             "L223.LaborForceFillout_GRIDR",
             "L223.Supplysector_elec_CHINA",
             "L223.ElecReserve_CHINA",
             "L223.SubsectorLogit_elec_CHINA",
             "L223.SubsectorShrwtFllt_elec_CHINA",
             "L223.SubsectorShrwt_nuc_CHINA",
             "L223.SubsectorShrwt_renew_CHINA",
             "L223.SubsectorInterp_elec_CHINA",
             "L223.SubsectorInterpTo_elec_CHINA",
             "L223.StubTech_elec_CHINA",
             "L223.StubTechEff_elec_CHINA",
             "L223.StubTechCapFactor_elec_CHINA",
             "L223.StubTechFixOut_elec_CHINA",
             "L223.StubTechFixOut_hydro_CHINA",
             "L223.StubTechProd_elec_CHINA",
             "L223.StubTechMarket_elec_CHINA",
             "L223.StubTechMarket_backup_CHINA",
             "L223.StubTechElecMarket_backup_CHINA",
             "L223.StubTechCapFactor_elec_wind_CHINA",
             "L223.StubTechCapFactor_elec_solar_CHINA",
             #"L223.DeleteSubsector_CHINAelec",
             #"L223.Supplysector_CHINAelec",
             #"L223.SubsectorShrwtFllt_CHINAelec",
             #"L223.SubsectorInterp_CHINAelec",
             #"L223.SubsectorLogit_CHINAelec",
             #"L223.TechShrwt_CHINAelec",
             #"L223.TechCoef_CHINAelec",
             #"L223.Production_CHINAelec",
             "L2232.DeleteSupplysector_CHINAelec",
             "L2232.Supplysector_CHINAelec",
             "L2232.SubsectorShrwtFllt_CHINAelec",
             "L2232.SubsectorInterp_CHINAelec",
             "L2232.SubsectorLogit_CHINAelec",
             "L2232.TechShrwt_CHINAelec",
             "L2232.TechCoef_CHINAelec",
             "L2232.Production_exports_CHINAelec",
             "L2232.Supplysector_elec_GRIDR",
             "L2232.ElecReserve_GRIDR",
             "L2232.SubsectorShrwtFllt_elec_GRIDR",
             "L2232.SubsectorInterp_elec_GRIDR",
             "L2232.SubsectorLogit_elec_GRIDR",
             "L2232.TechShrwt_elec_GRIDR",
             "L2232.TechCoef_elec_GRIDR",
             "L2232.TechCoef_elecownuse_GRIDR",
             "L2232.Production_imports_GRIDR",
             "L2232.Production_elec_gen_GRIDR",
             "L2232.StubTechElecMarket_backup_CHINA"))


  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "electricity_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    passthrough.sector <- technology <- share.weight <- NULL # silence package check notes

    # Load required inputs


    L223.Supplysector_elec_GRIDR <- get_data(all_data, "L223.Supplysector_elec_GRIDR")
    L223.SubsectorShrwtFllt_elec_GRIDR <- get_data(all_data, "L223.SubsectorShrwtFllt_elec_GRIDR")
    L223.SubsectorInterp_elec_GRIDR <- get_data(all_data, "L223.SubsectorInterp_elec_GRIDR")
    L223.SubsectorLogit_elec_GRIDR <- get_data(all_data, "L223.SubsectorLogit_elec_GRIDR")
    L223.TechShrwt_elec_GRIDR <- get_data(all_data, "L223.TechShrwt_elec_GRIDR")
    L223.TechCoef_elec_GRIDR <- get_data(all_data, "L223.TechCoef_elec_GRIDR")
    L223.Production_elec_GRIDR <- get_data(all_data, "L223.Production_elec_GRIDR")
    L223.InterestRate_GRIDR <- get_data(all_data, "L223.InterestRate_GRIDR")
    L223.Pop_GRIDR <- get_data(all_data, "L223.Pop_GRIDR")
    L223.BaseGDP_GRIDR <- get_data(all_data, "L223.BaseGDP_GRIDR")
    L223.LaborForceFillout_GRIDR <- get_data(all_data, "L223.LaborForceFillout_GRIDR")
    L223.Supplysector_elec_CHINA <- get_data(all_data, "L223.Supplysector_elec_CHINA")
    L223.ElecReserve_CHINA <- get_data(all_data, "L223.ElecReserve_CHINA")
    L223.SubsectorLogit_elec_CHINA <- get_data(all_data, "L223.SubsectorLogit_elec_CHINA")
    L223.SubsectorShrwtFllt_elec_CHINA <- get_data(all_data, "L223.SubsectorShrwtFllt_elec_CHINA")
    L223.SubsectorShrwt_nuc_CHINA <- get_data(all_data, "L223.SubsectorShrwt_nuc_CHINA")
    L223.SubsectorShrwt_renew_CHINA <- get_data(all_data, "L223.SubsectorShrwt_renew_CHINA")
    L223.SubsectorInterp_elec_CHINA <- get_data(all_data, "L223.SubsectorInterp_elec_CHINA")
    L223.SubsectorInterpTo_elec_CHINA <- get_data(all_data, "L223.SubsectorInterpTo_elec_CHINA")
    L223.StubTech_elec_CHINA <- get_data(all_data, "L223.StubTech_elec_CHINA")
    L223.StubTechEff_elec_CHINA <- get_data(all_data, "L223.StubTechEff_elec_CHINA")
    L223.StubTechCapFactor_elec_CHINA <- get_data(all_data, "L223.StubTechCapFactor_elec_CHINA")
    L223.StubTechFixOut_elec_CHINA <- get_data(all_data, "L223.StubTechFixOut_elec_CHINA")
    L223.StubTechFixOut_hydro_CHINA <- get_data(all_data, "L223.StubTechFixOut_hydro_CHINA")
    L223.StubTechProd_elec_CHINA <- get_data(all_data, "L223.StubTechProd_elec_CHINA")
    L223.StubTechMarket_elec_CHINA <- get_data(all_data, "L223.StubTechMarket_elec_CHINA")
    L223.StubTechMarket_backup_CHINA <- get_data(all_data, "L223.StubTechMarket_backup_CHINA")
    L223.StubTechElecMarket_backup_CHINA <- get_data(all_data, "L223.StubTechElecMarket_backup_CHINA")
    L223.StubTechCapFactor_elec_wind_CHINA <- get_data(all_data, "L223.StubTechCapFactor_elec_wind_CHINA")
    L223.StubTechCapFactor_elec_solar_CHINA <- get_data(all_data, "L223.StubTechCapFactor_elec_solar_CHINA")
    L2232.DeleteSupplysector_CHINAelec <- get_data(all_data, "L2232.DeleteSupplysector_CHINAelec")
    L2232.Supplysector_CHINAelec <- get_data(all_data, "L2232.Supplysector_CHINAelec")
    L2232.SubsectorShrwtFllt_CHINAelec <- get_data(all_data, "L2232.SubsectorShrwtFllt_CHINAelec")
    L2232.SubsectorInterp_CHINAelec <- get_data(all_data, "L2232.SubsectorInterp_CHINAelec")
    L2232.SubsectorLogit_CHINAelec <- get_data(all_data, "L2232.SubsectorLogit_CHINAelec")
    L2232.TechShrwt_CHINAelec <- get_data(all_data, "L2232.TechShrwt_CHINAelec")
    L2232.TechCoef_CHINAelec <- get_data(all_data, "L2232.TechCoef_CHINAelec")
    L2232.Production_exports_CHINAelec <- get_data(all_data, "L2232.Production_exports_CHINAelec")
    L2232.Supplysector_elec_GRIDR <- get_data(all_data, "L2232.Supplysector_elec_GRIDR")
    L2232.ElecReserve_GRIDR <- get_data(all_data, "L2232.ElecReserve_GRIDR")
    L2232.SubsectorShrwtFllt_elec_GRIDR <- get_data(all_data, "L2232.SubsectorShrwtFllt_elec_GRIDR")
    L2232.SubsectorInterp_elec_GRIDR <- get_data(all_data, "L2232.SubsectorInterp_elec_GRIDR")
    L2232.SubsectorLogit_elec_GRIDR <- get_data(all_data, "L2232.SubsectorLogit_elec_GRIDR")
    L2232.TechShrwt_elec_GRIDR <- get_data(all_data, "L2232.TechShrwt_elec_GRIDR")
    L2232.TechCoef_elec_GRIDR <- get_data(all_data, "L2232.TechCoef_elec_GRIDR")
    L2232.TechCoef_elecownuse_GRIDR <- get_data(all_data, "L2232.TechCoef_elecownuse_GRIDR")
    L2232.Production_imports_GRIDR <- get_data(all_data, "L2232.Production_imports_GRIDR")
    L2232.Production_elec_gen_GRIDR <- get_data(all_data, "L2232.Production_elec_gen_GRIDR")
    L2232.StubTechElecMarket_backup_CHINA <- get_data(all_data, "L2232.StubTechElecMarket_backup_CHINA")

    # ===================================================
    # Rename tibble columns to match the L2 data names.
    L223.StubTechProd_elec_CHINA      <- rename(L223.StubTechProd_elec_CHINA, tech.share.weight = share.weight)

    # Produce outputs
    create_xml("electricity_CHINA.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_logit_tables_xml(L223.Supplysector_elec_GRIDR, "Supplysector") %>%
      add_xml_data(L223.SubsectorShrwtFllt_elec_GRIDR, "SubsectorShrwtFllt") %>%
      add_xml_data(L223.SubsectorInterp_elec_GRIDR, "SubsectorInterp") %>%
      add_logit_tables_xml(L223.SubsectorLogit_elec_GRIDR, "SubsectorLogit") %>%
      add_xml_data(L223.TechShrwt_elec_GRIDR, "TechShrwt") %>%
      add_xml_data(L223.TechCoef_elec_GRIDR, "TechCoef") %>%
      add_xml_data(L223.Production_elec_GRIDR, "Production") %>%
      add_xml_data(L223.InterestRate_GRIDR, "InterestRate") %>%
      add_xml_data(L223.Pop_GRIDR, "Pop") %>%
      add_xml_data(L223.BaseGDP_GRIDR, "BaseGDP") %>%
      add_xml_data(L223.LaborForceFillout_GRIDR, "LaborForceFillout") %>%
      add_logit_tables_xml(L223.Supplysector_elec_CHINA, "Supplysector") %>%
      add_xml_data(L223.ElecReserve_CHINA, "ElecReserve") %>%
      add_logit_tables_xml(L223.SubsectorLogit_elec_CHINA, "SubsectorLogit") %>%
      add_xml_data(L223.SubsectorShrwtFllt_elec_CHINA, "SubsectorShrwtFllt") %>%
      add_xml_data(L223.SubsectorShrwt_nuc_CHINA, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorShrwt_renew_CHINA, "SubsectorShrwt") %>%
      add_xml_data(L223.SubsectorInterp_elec_CHINA, "SubsectorInterp") %>%
      add_xml_data(L223.SubsectorInterpTo_elec_CHINA, "SubsectorInterpTo") %>%
      add_xml_data(L223.StubTech_elec_CHINA, "StubTech") %>%
      add_xml_data(L223.StubTechEff_elec_CHINA, "StubTechEff") %>%
      add_xml_data(L223.StubTechCapFactor_elec_CHINA, "StubTechCapFactor") %>%
      add_xml_data(L223.StubTechFixOut_elec_CHINA, "StubTechFixOut") %>%
      add_xml_data(L223.StubTechFixOut_hydro_CHINA, "StubTechFixOut") %>%
      add_xml_data(L223.StubTechProd_elec_CHINA, "StubTechProd") %>%
      add_xml_data(L223.StubTechMarket_elec_CHINA, "StubTechMarket") %>%
      add_xml_data(L223.StubTechMarket_backup_CHINA, "StubTechMarket") %>%
      add_xml_data(L223.StubTechElecMarket_backup_CHINA, "StubTechElecMarket") %>%
      add_xml_data(L223.StubTechCapFactor_elec_wind_CHINA, "StubTechCapFactor") %>%
      add_xml_data(L223.StubTechCapFactor_elec_solar_CHINA, "StubTechCapFactor") %>%
      add_xml_data(L2232.DeleteSupplysector_CHINAelec, "DeleteSupplysector") %>%
      add_logit_tables_xml(L2232.Supplysector_CHINAelec, "Supplysector") %>%
      add_xml_data(L2232.SubsectorShrwtFllt_CHINAelec, "SubsectorShrwtFllt") %>%
      add_xml_data(L2232.SubsectorInterp_CHINAelec, "SubsectorInterp") %>%
      add_logit_tables_xml(L2232.SubsectorLogit_CHINAelec, "SubsectorLogit") %>%
      add_xml_data(L2232.TechShrwt_CHINAelec, "TechShrwt") %>%
      add_xml_data(L2232.TechCoef_CHINAelec, "TechCoef") %>%
      add_xml_data(L2232.Production_exports_CHINAelec, "Production") %>%
      add_logit_tables_xml(L2232.Supplysector_elec_GRIDR, "Supplysector") %>%
      add_xml_data(L2232.ElecReserve_GRIDR, "ElecReserve") %>%
      add_xml_data(L2232.SubsectorShrwtFllt_elec_GRIDR, "SubsectorShrwtFllt") %>%
      add_xml_data(L2232.SubsectorInterp_elec_GRIDR, "SubsectorInterp") %>%
      add_logit_tables_xml(L2232.SubsectorLogit_elec_GRIDR, "SubsectorLogit") %>%
      add_xml_data(L2232.TechShrwt_elec_GRIDR, "TechShrwt") %>%
      add_xml_data(L2232.TechCoef_elec_GRIDR, "TechCoef") %>%
      add_xml_data(L2232.TechCoef_elecownuse_GRIDR, "TechCoef") %>%
      add_xml_data(L2232.Production_imports_GRIDR, "Production") %>%
      add_xml_data(L2232.Production_elec_gen_GRIDR, "Production") %>%
      add_xml_data(L2232.StubTechElecMarket_backup_CHINA, "StubTechElecMarket") %>%
      add_precursors("L223.Supplysector_elec_GRIDR",
                     "L223.SubsectorShrwtFllt_elec_GRIDR",
                     "L223.SubsectorInterp_elec_GRIDR",
                     "L223.SubsectorLogit_elec_GRIDR",
                     "L223.TechShrwt_elec_GRIDR",
                     "L223.TechCoef_elec_GRIDR",
                     "L223.Production_elec_GRIDR",
                     "L223.InterestRate_GRIDR",
                     "L223.Pop_GRIDR",
                     "L223.BaseGDP_GRIDR",
                     "L223.LaborForceFillout_GRIDR",
                     "L223.Supplysector_elec_CHINA",
                     "L223.ElecReserve_CHINA",
                     "L223.SubsectorLogit_elec_CHINA",
                     "L223.SubsectorShrwtFllt_elec_CHINA",
                     "L223.SubsectorShrwt_nuc_CHINA",
                     "L223.SubsectorShrwt_renew_CHINA",
                     "L223.SubsectorInterp_elec_CHINA",
                     "L223.SubsectorInterpTo_elec_CHINA",
                     "L223.StubTech_elec_CHINA",
                     "L223.StubTechEff_elec_CHINA",
                     "L223.StubTechCapFactor_elec_CHINA",
                     "L223.StubTechFixOut_elec_CHINA",
                     "L223.StubTechFixOut_hydro_CHINA",
                     "L223.StubTechProd_elec_CHINA",
                     "L223.StubTechMarket_elec_CHINA",
                     "L223.StubTechMarket_backup_CHINA",
                     "L223.StubTechElecMarket_backup_CHINA",
                     "L223.StubTechCapFactor_elec_wind_CHINA",
                     "L223.StubTechCapFactor_elec_solar_CHINA",
                     "L2232.DeleteSupplysector_CHINAelec",
                     "L2232.Supplysector_CHINAelec",
                     "L2232.SubsectorShrwtFllt_CHINAelec",
                     "L2232.SubsectorInterp_CHINAelec",
                     "L2232.SubsectorLogit_CHINAelec",
                     "L2232.TechShrwt_CHINAelec",
                     "L2232.TechCoef_CHINAelec",
                     "L2232.Production_exports_CHINAelec",
                     "L2232.Supplysector_elec_GRIDR",
                     "L2232.ElecReserve_GRIDR",
                     "L2232.SubsectorShrwtFllt_elec_GRIDR",
                     "L2232.SubsectorInterp_elec_GRIDR",
                     "L2232.SubsectorLogit_elec_GRIDR",
                     "L2232.TechShrwt_elec_GRIDR",
                     "L2232.TechCoef_elec_GRIDR",
                     "L2232.TechCoef_elecownuse_GRIDR",
                     "L2232.Production_imports_GRIDR",
                     "L2232.Production_elec_gen_GRIDR",
                     "L2232.StubTechElecMarket_backup_CHINA") ->
      electricity_CHINA.xml

    return_data(electricity_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
