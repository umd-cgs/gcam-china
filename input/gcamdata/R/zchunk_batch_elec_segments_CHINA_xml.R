# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_batch_elec_segments_CHINA_xml
#'
#' Construct XML data structure for \code{elec_segments_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{elec_segments_USA.xml}. The corresponding file in the
#' original data system was \code{batch_elec_segments_USA.xml} (gcamusa xml-batch).
module_gcamchina_batch_elec_segments_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2234.Supplysector_elecS_CHINA",
             "L2234.ElecReserve_elecS_CHINA",
             "L2234.SubsectorLogit_elecS_CHINA",
             "L2234.SubsectorShrwtInterp_elecS_CHINA",
             "L2234.SubsectorShrwtInterpTo_elecS_CHINA",
             "L2234.SubsectorShrwt_elecS_CHINA",
             "L2234.StubTechEff_elecS_CHINA",
             "L2234.StubTechCapFactor_elecS_solar_CHINA",
             "L2234.StubTechCapFactor_elecS_wind_CHINA",
             "L2234.SubsectorShrwtFllt_elecS_grid_CHINA",
             "L2234.SubsectorShrwtInterp_elecS_grid_CHINA",
             "L2234.PassThroughSector_elecS_CHINA",
             "L2234.PassThroughTech_elecS_grid_CHINA",
             "L2234.GlobalTechShrwt_elecS_CHINA",
             "L2234.GlobalIntTechShrwt_elecS_CHINA",
             "L2234.PrimaryRenewKeyword_elecS_CHINA",
             "L2234.PrimaryRenewKeywordInt_elecS_CHINA",
             "L2234.AvgFossilEffKeyword_elecS_CHINA",
             "L2234.GlobalTechCapital_elecS_CHINA",
             "L2234.GlobalIntTechCapital_elecS_CHINA",
             "L2234.GlobalTechOMfixed_elecS_CHINA",
             "L2234.GlobalIntTechOMfixed_elecS_CHINA",
             "L2234.GlobalTechOMvar_elecS_CHINA",
             "L2234.GlobalIntTechOMvar_elecS_CHINA",
             "L2234.GlobalTechCapFac_elecS_CHINA",
             "L2234.GlobalTechEff_elecS_CHINA",
             "L2234.GlobalIntTechEff_elecS_CHINA",
             "L2234.GlobalTechLifetime_elecS_CHINA",
             "L2234.GlobalIntTechLifetime_elecS_CHINA",
             "L2234.GlobalTechProfitShutdown_elecS_CHINA",
             "L2234.GlobalTechSCurve_elecS_CHINA",
             "L2234.GlobalTechCapture_elecS_CHINA",
             "L2234.GlobalIntTechBackup_elecS_CHINA",
             "L2234.StubTechMarket_elecS_CHINA",
             "L2234.StubTechMarket_backup_elecS_CHINA",
             "L2234.StubTechElecMarket_backup_elecS_CHINA",
             "L2234.StubTechProd_elecS_CHINA",
             "L2234.StubTechFixOut_elecS_CHINA",
             "L2234.StubTechFixOut_hydro_elecS_CHINA",
             "L2234.TechShrwt_elecS_grid_CHINA",
             "L2234.TechCoef_elecS_grid_CHINA",
             "L2234.TechProd_elecS_grid_CHINA",
             "L2235.DeleteSupplysector_elec_CHINA",
             "L2235.InterestRate_GRID_CHINA",
             "L2235.Pop_GRID_CHINA",
             "L2235.BaseGDP_GRID_CHINA",
             "L2235.LaborForceFillout_GRID_CHINA",
             "L2235.Supplysector_elec_CHINA",
             "L2235.ElecReserve_elecS_grid_vertical_CHINA",
             "L2235.SubsectorLogit_elec_CHINA",
             "L2235.SubsectorShrwtFllt_elec_CHINA",
             "L2235.SubsectorInterp_elec_CHINA",
             "L2235.SubsectorShrwtFllt_elecS_grid_vertical_CHINA",
             "L2235.SubsectorShrwtInterp_elecS_grid_vertical_CHINA",
             "L2235.TechShrwt_elec_CHINA",
             "L2235.TechCoef_elec_CHINA",
             "L2235.Production_exports_elec_CHINA",
             "L2235.TechShrwt_elecS_grid_vertical_CHINA",
             "L2235.TechCoef_elecS_grid_vertical_CHINA",
             "L2235.Supplysector_elec_GRID_CHINA",
             "L2235.SubsectorLogit_elec_GRID_CHINA",
             "L2235.SubsectorShrwtFllt_elec_GRID_CHINA",
             "L2235.SubsectorInterp_elec_GRID_CHINA",
             "L2235.TechShrwt_elec_GRID_CHINA",
             "L2235.TechCoef_elec_GRID_CHINA",
             "L2235.TechCoef_elecownuse_GRID_CHINA",
             "L2235.Production_imports_GRID_CHINA",
             "L2235.Production_elec_gen_GRID_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "elec_segments_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    passthrough.sector <- technology <- share.weight <- intermittent.technology <- supplysector <- subsector <- NULL
    # silence package check notes

    # Load required inputs
    L2234.Supplysector_elecS_CHINA <- get_data(all_data, "L2234.Supplysector_elecS_CHINA")
    L2234.ElecReserve_elecS_CHINA <- get_data(all_data, "L2234.ElecReserve_elecS_CHINA")
    L2234.SubsectorLogit_elecS_CHINA <- get_data(all_data, "L2234.SubsectorLogit_elecS_CHINA")
    L2234.SubsectorShrwtInterp_elecS_CHINA <- get_data(all_data, "L2234.SubsectorShrwtInterp_elecS_CHINA")
    L2234.SubsectorShrwtInterpTo_elecS_CHINA <- get_data(all_data, "L2234.SubsectorShrwtInterpTo_elecS_CHINA")
    L2234.SubsectorShrwt_elecS_CHINA <- get_data(all_data, "L2234.SubsectorShrwt_elecS_CHINA")
    L2234.StubTechEff_elecS_CHINA <- get_data(all_data, "L2234.StubTechEff_elecS_CHINA")
    L2234.StubTechCapFactor_elecS_solar_CHINA <- get_data(all_data, "L2234.StubTechCapFactor_elecS_solar_CHINA")
    L2234.StubTechCapFactor_elecS_wind_CHINA <- get_data(all_data, "L2234.StubTechCapFactor_elecS_wind_CHINA")
    L2234.SubsectorShrwtFllt_elecS_grid_CHINA <- get_data(all_data, "L2234.SubsectorShrwtFllt_elecS_grid_CHINA")
    L2234.SubsectorShrwtInterp_elecS_grid_CHINA <- get_data(all_data, "L2234.SubsectorShrwtInterp_elecS_grid_CHINA")
    L2234.PassThroughSector_elecS_CHINA <- get_data(all_data, "L2234.PassThroughSector_elecS_CHINA")
    L2234.PassThroughTech_elecS_grid_CHINA <- get_data(all_data, "L2234.PassThroughTech_elecS_grid_CHINA")
    L2234.GlobalTechShrwt_elecS_CHINA <- get_data(all_data, "L2234.GlobalTechShrwt_elecS_CHINA")
    L2234.GlobalIntTechShrwt_elecS_CHINA <- get_data(all_data, "L2234.GlobalIntTechShrwt_elecS_CHINA")
    L2234.PrimaryRenewKeyword_elecS_CHINA <- get_data(all_data, "L2234.PrimaryRenewKeyword_elecS_CHINA")
    L2234.PrimaryRenewKeywordInt_elecS_CHINA <- get_data(all_data, "L2234.PrimaryRenewKeywordInt_elecS_CHINA")
    L2234.AvgFossilEffKeyword_elecS_CHINA <- get_data(all_data, "L2234.AvgFossilEffKeyword_elecS_CHINA")
    L2234.GlobalTechCapital_elecS_CHINA <- get_data(all_data, "L2234.GlobalTechCapital_elecS_CHINA")
    L2234.GlobalIntTechCapital_elecS_CHINA <- get_data(all_data, "L2234.GlobalIntTechCapital_elecS_CHINA")
    L2234.GlobalTechOMfixed_elecS_CHINA <- get_data(all_data, "L2234.GlobalTechOMfixed_elecS_CHINA")
    L2234.GlobalIntTechOMfixed_elecS_CHINA <- get_data(all_data, "L2234.GlobalIntTechOMfixed_elecS_CHINA")
    L2234.GlobalTechOMvar_elecS_CHINA <- get_data(all_data, "L2234.GlobalTechOMvar_elecS_CHINA")
    L2234.GlobalIntTechOMvar_elecS_CHINA <- get_data(all_data, "L2234.GlobalIntTechOMvar_elecS_CHINA")
    L2234.GlobalTechCapFac_elecS_CHINA <- get_data(all_data, "L2234.GlobalTechCapFac_elecS_CHINA")
    L2234.GlobalTechEff_elecS_CHINA <- get_data(all_data, "L2234.GlobalTechEff_elecS_CHINA")
    L2234.GlobalIntTechEff_elecS_CHINA <- get_data(all_data, "L2234.GlobalIntTechEff_elecS_CHINA")
    L2234.GlobalTechLifetime_elecS_CHINA <- get_data(all_data, "L2234.GlobalTechLifetime_elecS_CHINA")
    L2234.GlobalIntTechLifetime_elecS_CHINA <- get_data(all_data, "L2234.GlobalIntTechLifetime_elecS_CHINA")
    L2234.GlobalTechProfitShutdown_elecS_CHINA <- get_data(all_data, "L2234.GlobalTechProfitShutdown_elecS_CHINA")
    L2234.GlobalTechSCurve_elecS_CHINA <- get_data(all_data, "L2234.GlobalTechSCurve_elecS_CHINA")
    L2234.GlobalTechCapture_elecS_CHINA <- get_data(all_data, "L2234.GlobalTechCapture_elecS_CHINA")
    L2234.GlobalIntTechBackup_elecS_CHINA <- get_data(all_data, "L2234.GlobalIntTechBackup_elecS_CHINA")
    L2234.StubTechMarket_elecS_CHINA <- get_data(all_data, "L2234.StubTechMarket_elecS_CHINA")
    L2234.StubTechMarket_backup_elecS_CHINA <- get_data(all_data, "L2234.StubTechMarket_backup_elecS_CHINA")
    L2234.StubTechElecMarket_backup_elecS_CHINA <- get_data(all_data, "L2234.StubTechElecMarket_backup_elecS_CHINA")
    L2234.StubTechProd_elecS_CHINA <- get_data(all_data, "L2234.StubTechProd_elecS_CHINA")
    L2234.StubTechFixOut_elecS_CHINA <- get_data(all_data, "L2234.StubTechFixOut_elecS_CHINA")
    L2234.StubTechFixOut_hydro_elecS_CHINA <- get_data(all_data, "L2234.StubTechFixOut_hydro_elecS_CHINA")
    L2234.TechShrwt_elecS_grid_CHINA <- get_data(all_data, "L2234.TechShrwt_elecS_grid_CHINA")
    L2234.TechCoef_elecS_grid_CHINA <- get_data(all_data, "L2234.TechCoef_elecS_grid_CHINA")
    L2234.TechProd_elecS_grid_CHINA <- get_data(all_data, "L2234.TechProd_elecS_grid_CHINA")

    L2235.DeleteSupplysector_elec_CHINA <- get_data(all_data, "L2235.DeleteSupplysector_elec_CHINA")
    L2235.InterestRate_GRID_CHINA <- get_data(all_data, "L2235.InterestRate_GRID_CHINA")
    L2235.Pop_GRID_CHINA <- get_data(all_data, "L2235.Pop_GRID_CHINA")
    L2235.BaseGDP_GRID_CHINA <- get_data(all_data, "L2235.BaseGDP_GRID_CHINA")
    L2235.LaborForceFillout_GRID_CHINA <- get_data(all_data, "L2235.LaborForceFillout_GRID_CHINA")
    L2235.Supplysector_elec_CHINA <- get_data(all_data, "L2235.Supplysector_elec_CHINA")
    L2235.ElecReserve_elecS_grid_vertical_CHINA <- get_data(all_data, "L2235.ElecReserve_elecS_grid_vertical_CHINA")
    L2235.SubsectorLogit_elec_CHINA <- get_data(all_data, "L2235.SubsectorLogit_elec_CHINA")
    L2235.SubsectorShrwtFllt_elec_CHINA <- get_data(all_data, "L2235.SubsectorShrwtFllt_elec_CHINA")
    L2235.SubsectorInterp_elec_CHINA <- get_data(all_data, "L2235.SubsectorInterp_elec_CHINA")
    L2235.SubsectorShrwtFllt_elecS_grid_vertical_CHINA <- get_data(all_data, "L2235.SubsectorShrwtFllt_elecS_grid_vertical_CHINA")
    L2235.SubsectorShrwtInterp_elecS_grid_vertical_CHINA <- get_data(all_data, "L2235.SubsectorShrwtInterp_elecS_grid_vertical_CHINA")
    L2235.TechShrwt_elec_CHINA <- get_data(all_data, "L2235.TechShrwt_elec_CHINA")
    L2235.TechCoef_elec_CHINA <- get_data(all_data, "L2235.TechCoef_elec_CHINA")
    L2235.Production_exports_elec_CHINA <- get_data(all_data, "L2235.Production_exports_elec_CHINA")
    L2235.TechShrwt_elecS_grid_vertical_CHINA <- get_data(all_data, "L2235.TechShrwt_elecS_grid_vertical_CHINA")
    L2235.TechCoef_elecS_grid_vertical_CHINA <- get_data(all_data, "L2235.TechCoef_elecS_grid_vertical_CHINA")
    L2235.Supplysector_elec_GRID_CHINA <- get_data(all_data, "L2235.Supplysector_elec_GRID_CHINA")
    L2235.SubsectorLogit_elec_GRID_CHINA <- get_data(all_data, "L2235.SubsectorLogit_elec_GRID_CHINA")
    L2235.SubsectorShrwtFllt_elec_GRID_CHINA <- get_data(all_data, "L2235.SubsectorShrwtFllt_elec_GRID_CHINA")
    L2235.SubsectorInterp_elec_GRID_CHINA <- get_data(all_data, "L2235.SubsectorInterp_elec_GRID_CHINA")
    L2235.TechShrwt_elec_GRID_CHINA <- get_data(all_data, "L2235.TechShrwt_elec_GRID_CHINA")
    L2235.TechCoef_elec_GRID_CHINA <- get_data(all_data, "L2235.TechCoef_elec_GRID_CHINA")
    L2235.TechCoef_elecownuse_GRID_CHINA <- get_data(all_data, "L2235.TechCoef_elecownuse_GRID_CHINA")
    L2235.Production_imports_GRID_CHINA <- get_data(all_data, "L2235.Production_imports_GRID_CHINA")
    L2235.Production_elec_gen_GRID_CHINA <- get_data(all_data, "L2235.Production_elec_gen_GRID_CHINA")


    # ===================================================
    # Rename tibble columns to match the L2 data names.
    L2234.PassThroughSector_elecS_CHINA <- rename(L2234.PassThroughSector_elecS_CHINA, pass.through.sector = passthrough.sector)
    L2234.PassThroughTech_elecS_grid_CHINA <- rename(L2234.PassThroughTech_elecS_grid_CHINA, pass.through.technology = technology)
    # NOTE:  below is an issue with LEVEL2_DATA_NAMES... PrimaryRenewKeywordInt name should be intermittent.technology,
    # as the table is for intermittent technologies and the old DS MI header name is intermittent.technology
    L2234.PrimaryRenewKeywordInt_elecS_CHINA <- rename(L2234.PrimaryRenewKeywordInt_elecS_CHINA, technology = intermittent.technology)

    # Function to fix GlobalTech / GlobalIntTech sector & subsector names, which is a recurring issue
    fix_global_tech_names <- function(data){
      data_new <- data %>%
        rename(sector.name = supplysector,
               subsector.name = subsector)
      return(data_new)
    }

    L2234.GlobalTechCapital_elecS_CHINA <- fix_global_tech_names(L2234.GlobalTechCapital_elecS_CHINA)
    L2234.GlobalIntTechCapital_elecS_CHINA <- fix_global_tech_names(L2234.GlobalIntTechCapital_elecS_CHINA)
    L2234.GlobalTechOMfixed_elecS_CHINA <- fix_global_tech_names(L2234.GlobalTechOMfixed_elecS_CHINA)
    L2234.GlobalIntTechOMfixed_elecS_CHINA <- fix_global_tech_names(L2234.GlobalIntTechOMfixed_elecS_CHINA)
    L2234.GlobalTechOMvar_elecS_CHINA <- fix_global_tech_names(L2234.GlobalTechOMvar_elecS_CHINA)
    L2234.GlobalIntTechOMvar_elecS_CHINA <- fix_global_tech_names(L2234.GlobalIntTechOMvar_elecS_CHINA)
    L2234.GlobalTechCapFac_elecS_CHINA <- fix_global_tech_names(L2234.GlobalTechCapFac_elecS_CHINA)
    L2234.GlobalTechEff_elecS_CHINA <- fix_global_tech_names(L2234.GlobalTechEff_elecS_CHINA)
    L2234.GlobalIntTechEff_elecS_CHINA <- fix_global_tech_names(L2234.GlobalIntTechEff_elecS_CHINA)
    L2234.GlobalTechLifetime_elecS_CHINA <- fix_global_tech_names(L2234.GlobalTechLifetime_elecS_CHINA)
    L2234.GlobalIntTechLifetime_elecS_CHINA <- fix_global_tech_names(L2234.GlobalIntTechLifetime_elecS_CHINA)
    L2234.GlobalTechProfitShutdown_elecS_CHINA <- fix_global_tech_names(L2234.GlobalTechProfitShutdown_elecS_CHINA)
    L2234.GlobalTechSCurve_elecS_CHINA <- fix_global_tech_names(L2234.GlobalTechSCurve_elecS_CHINA)
    L2234.GlobalTechCapture_elecS_CHINA <- fix_global_tech_names(L2234.GlobalTechCapture_elecS_CHINA)
    # NOTE:  below is an issue with LEVEL2_DATA_NAMES... GlobalIntTechBackup name should be intermittent.technology,
    # as the table is for intermittent technologies and the old DS MI header name is intermittent.technology
    L2234.GlobalIntTechBackup_elecS_CHINA <- L2234.GlobalIntTechBackup_elecS_CHINA %>%
      fix_global_tech_names() %>%
      rename(technology = intermittent.technology)

    L2234.StubTechProd_elecS_CHINA <- rename(L2234.StubTechProd_elecS_CHINA, tech.share.weight = share.weight)
    L2234.TechProd_elecS_grid_CHINA <- rename(L2234.TechProd_elecS_grid_CHINA, tech.share.weight = share.weight)


    # Produce outputs
    create_xml("elec_segments_CHINA.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L2234.PassThroughSector_elecS_CHINA, "PassThroughSector") %>%
      add_xml_data(L2234.PassThroughTech_elecS_grid_CHINA, "PassThroughTech") %>%
      add_logit_tables_xml(L2234.Supplysector_elecS_CHINA, "Supplysector") %>%
      add_xml_data(L2234.ElecReserve_elecS_CHINA, "ElecReserve") %>%
      add_logit_tables_xml(L2234.SubsectorLogit_elecS_CHINA, "SubsectorLogit") %>%
      add_xml_data(L2234.GlobalTechShrwt_elecS_CHINA, "GlobalTechShrwt") %>%
      add_xml_data(L2234.GlobalIntTechShrwt_elecS_CHINA, "GlobalIntTechShrwt") %>%
      add_xml_data(L2234.PrimaryRenewKeyword_elecS_CHINA, "PrimaryRenewKeyword") %>%
      add_xml_data(L2234.PrimaryRenewKeywordInt_elecS_CHINA, "PrimaryRenewKeywordInt") %>%
      add_xml_data(L2234.AvgFossilEffKeyword_elecS_CHINA, "AvgFossilEffKeyword") %>%
      add_xml_data(L2234.GlobalTechCapital_elecS_CHINA, "GlobalTechCapital") %>%
      add_xml_data(L2234.GlobalIntTechCapital_elecS_CHINA, "GlobalIntTechCapital") %>%
      add_xml_data(L2234.GlobalTechOMfixed_elecS_CHINA, "GlobalTechOMfixed") %>%
      add_xml_data(L2234.GlobalIntTechOMfixed_elecS_CHINA, "GlobalIntTechOMfixed") %>%
      add_xml_data(L2234.GlobalTechOMvar_elecS_CHINA, "GlobalTechOMvar") %>%
      add_xml_data(L2234.GlobalIntTechOMvar_elecS_CHINA, "GlobalIntTechOMvar") %>%
      add_xml_data(L2234.GlobalTechCapFac_elecS_CHINA, "GlobalTechCapFac") %>%
      add_xml_data(L2234.GlobalTechEff_elecS_CHINA, "GlobalTechEff") %>%
      add_xml_data(L2234.GlobalIntTechEff_elecS_CHINA, "GlobalIntTechEff") %>%
      add_xml_data(L2234.GlobalTechLifetime_elecS_CHINA, "GlobalTechLifetime") %>%
      add_xml_data(L2234.GlobalIntTechLifetime_elecS_CHINA, "GlobalIntTechLifetime") %>%
      add_xml_data(L2234.GlobalTechProfitShutdown_elecS_CHINA, "GlobalTechProfitShutdown") %>%
      add_xml_data(L2234.GlobalTechSCurve_elecS_CHINA, "GlobalTechSCurve") %>%
      add_xml_data(L2234.GlobalTechCapture_elecS_CHINA, "GlobalTechCapture") %>%
      add_xml_data(L2234.GlobalIntTechBackup_elecS_CHINA, "GlobalIntTechBackup") %>%
      add_xml_data(L2234.StubTechMarket_elecS_CHINA, "StubTechMarket") %>%
      add_xml_data(L2234.StubTechMarket_backup_elecS_CHINA, "StubTechMarket") %>%
      add_xml_data(L2234.StubTechElecMarket_backup_elecS_CHINA, "StubTechElecMarket") %>%
      add_xml_data(L2234.StubTechProd_elecS_CHINA, "StubTechProd") %>%
      add_xml_data(L2234.SubsectorShrwt_elecS_CHINA, "SubsectorShrwt") %>%
      add_xml_data(L2234.SubsectorShrwtInterp_elecS_CHINA, "SubsectorInterp") %>%
      add_xml_data(L2234.SubsectorShrwtInterpTo_elecS_CHINA, "SubsectorInterpTo") %>%
      add_xml_data(L2234.StubTechCapFactor_elecS_wind_CHINA, "StubTechCapFactor") %>%
      add_xml_data(L2234.StubTechCapFactor_elecS_solar_CHINA, "StubTechCapFactor") %>%
      add_xml_data(L2234.StubTechFixOut_elecS_CHINA, "StubTechFixOut") %>%
      add_xml_data(L2234.StubTechEff_elecS_CHINA, "StubTechEff") %>%
      add_xml_data(L2234.StubTechFixOut_hydro_elecS_CHINA, "StubTechFixOut") %>%
      add_xml_data(L2234.TechShrwt_elecS_grid_CHINA, "TechShrwt") %>%
      add_xml_data(L2234.TechCoef_elecS_grid_CHINA, "TechCoef") %>%
      add_xml_data(L2234.TechProd_elecS_grid_CHINA, "Production") %>%
      add_xml_data(L2234.SubsectorShrwtFllt_elecS_grid_CHINA, "SubsectorShrwtFllt") %>%
      add_xml_data(L2234.SubsectorShrwtInterp_elecS_grid_CHINA, "SubsectorInterp") %>%
      add_xml_data(L2235.DeleteSupplysector_elec_CHINA, "DeleteSupplysector") %>%
      add_xml_data(L2235.InterestRate_GRID_CHINA, "InterestRate") %>%
      add_xml_data(L2235.Pop_GRID_CHINA, "Pop") %>%
      add_xml_data(L2235.BaseGDP_GRID_CHINA, "BaseGDP") %>%
      add_xml_data(L2235.LaborForceFillout_GRID_CHINA, "LaborForceFillout") %>%
      add_logit_tables_xml(L2235.Supplysector_elec_CHINA, "Supplysector") %>%
      add_xml_data(L2235.ElecReserve_elecS_grid_vertical_CHINA, "ElecReserve") %>%
      add_logit_tables_xml(L2235.SubsectorLogit_elec_CHINA, "SubsectorLogit") %>%
      add_xml_data(L2235.SubsectorShrwtFllt_elec_CHINA, "SubsectorShrwtFllt") %>%
      add_xml_data(L2235.SubsectorInterp_elec_CHINA, "SubsectorInterp") %>%
      add_xml_data(L2235.SubsectorShrwtFllt_elecS_grid_vertical_CHINA, "SubsectorShrwtFllt") %>%
      add_xml_data(L2235.SubsectorShrwtInterp_elecS_grid_vertical_CHINA, "SubsectorInterp") %>%
      add_xml_data(L2235.TechShrwt_elec_CHINA, "TechShrwt") %>%
      add_xml_data(L2235.TechCoef_elec_CHINA, "TechCoef") %>%
      add_xml_data(L2235.Production_exports_elec_CHINA, "Production") %>%
      add_xml_data(L2235.TechShrwt_elecS_grid_vertical_CHINA, "TechShrwt") %>%
      add_xml_data(L2235.TechCoef_elecS_grid_vertical_CHINA, "TechCoef") %>%
      add_logit_tables_xml(L2235.Supplysector_elec_GRID_CHINA, "Supplysector") %>%
      add_logit_tables_xml(L2235.SubsectorLogit_elec_GRID_CHINA, "SubsectorLogit") %>%
      add_xml_data(L2235.SubsectorShrwtFllt_elec_GRID_CHINA, "SubsectorShrwtFllt") %>%
      add_xml_data(L2235.SubsectorInterp_elec_GRID_CHINA, "SubsectorInterp") %>%
      add_xml_data(L2235.TechShrwt_elec_GRID_CHINA, "TechShrwt") %>%
      add_xml_data(L2235.TechCoef_elec_GRID_CHINA, "TechCoef") %>%
      add_xml_data(L2235.TechCoef_elecownuse_GRID_CHINA, "TechCoef") %>%
      add_xml_data(L2235.Production_imports_GRID_CHINA, "Production") %>%
      add_xml_data(L2235.Production_elec_gen_GRID_CHINA, "Production") %>%
      add_precursors("L2234.Supplysector_elecS_CHINA",
                     "L2234.ElecReserve_elecS_CHINA",
                     "L2234.SubsectorLogit_elecS_CHINA",
                     "L2234.SubsectorShrwtInterp_elecS_CHINA",
                     "L2234.SubsectorShrwtInterpTo_elecS_CHINA",
                     "L2234.SubsectorShrwt_elecS_CHINA",
                     "L2234.StubTechEff_elecS_CHINA",
                     "L2234.StubTechCapFactor_elecS_solar_CHINA",
                     "L2234.StubTechCapFactor_elecS_wind_CHINA",
                     "L2234.SubsectorShrwtFllt_elecS_grid_CHINA",
                     "L2234.SubsectorShrwtInterp_elecS_grid_CHINA",
                     "L2234.PassThroughSector_elecS_CHINA",
                     "L2234.PassThroughTech_elecS_grid_CHINA",
                     "L2234.GlobalTechShrwt_elecS_CHINA",
                     "L2234.GlobalIntTechShrwt_elecS_CHINA",
                     "L2234.PrimaryRenewKeyword_elecS_CHINA",
                     "L2234.PrimaryRenewKeywordInt_elecS_CHINA",
                     "L2234.AvgFossilEffKeyword_elecS_CHINA",
                     "L2234.GlobalTechCapital_elecS_CHINA",
                     "L2234.GlobalIntTechCapital_elecS_CHINA",
                     "L2234.GlobalTechOMfixed_elecS_CHINA",
                     "L2234.GlobalIntTechOMfixed_elecS_CHINA",
                     "L2234.GlobalTechOMvar_elecS_CHINA",
                     "L2234.GlobalIntTechOMvar_elecS_CHINA",
                     "L2234.GlobalTechCapFac_elecS_CHINA",
                     "L2234.GlobalTechEff_elecS_CHINA",
                     "L2234.GlobalIntTechEff_elecS_CHINA",
                     "L2234.GlobalTechLifetime_elecS_CHINA",
                     "L2234.GlobalIntTechLifetime_elecS_CHINA",
                     "L2234.GlobalTechProfitShutdown_elecS_CHINA",
                     "L2234.GlobalTechSCurve_elecS_CHINA",
                     "L2234.GlobalTechCapture_elecS_CHINA",
                     "L2234.GlobalIntTechBackup_elecS_CHINA",
                     "L2234.StubTechMarket_elecS_CHINA",
                     "L2234.StubTechMarket_backup_elecS_CHINA",
                     "L2234.StubTechElecMarket_backup_elecS_CHINA",
                     "L2234.StubTechProd_elecS_CHINA",
                     "L2234.StubTechFixOut_elecS_CHINA",
                     "L2234.StubTechFixOut_hydro_elecS_CHINA",
                     "L2234.TechShrwt_elecS_grid_CHINA",
                     "L2234.TechCoef_elecS_grid_CHINA",
                     "L2234.TechProd_elecS_grid_CHINA",
                     "L2235.DeleteSupplysector_elec_CHINA",
                     "L2235.InterestRate_GRID_CHINA",
                     "L2235.Pop_GRID_CHINA",
                     "L2235.BaseGDP_GRID_CHINA",
                     "L2235.LaborForceFillout_GRID_CHINA",
                     "L2235.Supplysector_elec_CHINA",
                     "L2235.ElecReserve_elecS_grid_vertical_CHINA",
                     "L2235.SubsectorLogit_elec_CHINA",
                     "L2235.SubsectorShrwtFllt_elec_CHINA",
                     "L2235.SubsectorInterp_elec_CHINA",
                     "L2235.SubsectorShrwtFllt_elecS_grid_vertical_CHINA",
                     "L2235.SubsectorShrwtInterp_elecS_grid_vertical_CHINA",
                     "L2235.TechShrwt_elec_CHINA",
                     "L2235.TechCoef_elec_CHINA",
                     "L2235.Production_exports_elec_CHINA",
                     "L2235.TechShrwt_elecS_grid_vertical_CHINA",
                     "L2235.TechCoef_elecS_grid_vertical_CHINA",
                     "L2235.Supplysector_elec_GRID_CHINA",
                     "L2235.SubsectorLogit_elec_GRID_CHINA",
                     "L2235.SubsectorShrwtFllt_elec_GRID_CHINA",
                     "L2235.SubsectorInterp_elec_GRID_CHINA",
                     "L2235.TechShrwt_elec_GRID_CHINA",
                     "L2235.TechCoef_elec_GRID_CHINA",
                     "L2235.TechCoef_elecownuse_GRID_CHINA",
                     "L2235.Production_imports_GRID_CHINA",
                     "L2235.Production_elec_gen_GRID_CHINA") ->
      elec_segments_CHINA.xml

    return_data(elec_segments_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
