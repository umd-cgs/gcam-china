# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_building_xml
#'
#' Construct XML data structure for \code{building_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_building_CHINA.xml} (gcamchina XML).
module_gcamchina_building_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L244.DeleteConsumer_CHINAbld",
             "L244.DeleteSupplysector_CHINAbld",
             "L244.SubregionalShares_gcamchina",
             "L244.PriceExp_IntGains_gcamchina",
             "L244.DemandFunction_serv_gcamchina",
             "L244.DemandFunction_flsp_gcamchina",
             "L244.GompFnParam_gcamchina",
             "L244.Floorspace_gcamchina",
             "L244.Satiation_flsp_gcamchina",
             "L244.Satiation_impedance_gcamchina",
             "L244.SatiationAdder_gcamchina",
             "L244.ShellConductance_bld_gcamchina",
             "L244.FinalEnergyKeyword_bld_gcamchina",
             "L244.Supplysector_bld_gcamchina",
             "L244.StubTech_bld_gcamchina",
             "L244.StubTechMarket_bld_gcamchina",
             "L244.GlobalTechEff_bld_gcamchina",
             "L244.StubTechCalInput_bld_gcamchina",
             "L244.GlobalTechShrwt_bld_gcamchina",
             "L244.GlobalTechInterpTo_bld_gcamchina",
             "L244.GlobalTechCost_bld_gcamchina",
             "L244.GlobalTechSCurve_bld_gcamchina",
             "L244.GlobalTechIntGainOutputRatio_gcamchina",
             "L244.ThermalBaseService_gcamchina",
             "L244.GenericBaseService_gcamchina",
             "L244.ThermalServiceSatiation_gcamchina",
             "L244.GenericServiceSatiation_gcamchina",
             "L244.Intgains_scalar_gcamchina",
             "L244.SubsectorShrwtFllt_bld_gcamchina",
             "L244.SubsectorInterp_bld_gcamchina",
             "L244.SubsectorInterpTo_bld_gcamchina",
             "L244.SubsectorLogit_bld_gcamchina",
             "L244.GenericServiceImpedance_gcamchina",
             "L244.GenericServiceCoef_gcamchina",
             "L244.GenericServiceAdder_gcamchina",
             "L244.ThermalServiceImpedance_gcamchina",
             "L244.ThermalServiceCoef_gcamchina",
             "L244.ThermalServiceAdder_gcamchina",
             "L244.GenericServicePrice_gcamchina",
             "L244.ThermalServicePrice_gcamchina",
             "L244.GenericBaseDens_gcamchina",
             "L244.ThermalBaseDens_gcamchina",
             "L244.DeleteThermalService_gcamchina",
             "L244.DeleteGenericService_gcamchina",
             "L244.FuelPrefElast_bld_gcamchina"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.DeleteConsumer_CHINAbld <- get_data(all_data, "L244.DeleteConsumer_CHINAbld")
    L244.DeleteSupplysector_CHINAbld <- get_data(all_data, "L244.DeleteSupplysector_CHINAbld")
    L244.SubregionalShares <- get_data(all_data, "L244.SubregionalShares_gcamchina")
    L244.PriceExp_IntGains <- get_data(all_data, "L244.PriceExp_IntGains_gcamchina")
    L244.Floorspace <- get_data(all_data, "L244.Floorspace_gcamchina")
    L244.DemandFunction_serv <- get_data(all_data, "L244.DemandFunction_serv_gcamchina")
    L244.DemandFunction_flsp <- get_data(all_data, "L244.DemandFunction_flsp_gcamchina")
    L244.Satiation_flsp <- get_data(all_data, "L244.Satiation_flsp_gcamchina")
    L244.SatiationAdder <- get_data(all_data, "L244.SatiationAdder_gcamchina")
    L244.ThermalBaseService <- get_data(all_data, "L244.ThermalBaseService_gcamchina")
    L244.GenericBaseService <- get_data(all_data, "L244.GenericBaseService_gcamchina")
    L244.ThermalServiceSatiation <- get_data(all_data, "L244.ThermalServiceSatiation_gcamchina")
    L244.GenericServiceSatiation <- get_data(all_data, "L244.GenericServiceSatiation_gcamchina")
    L244.Intgains_scalar <- get_data(all_data, "L244.Intgains_scalar_gcamchina")
    L244.ShellConductance_bld <- get_data(all_data, "L244.ShellConductance_bld_gcamchina")
    L244.Supplysector_bld <- get_data(all_data, "L244.Supplysector_bld_gcamchina")
    L244.FinalEnergyKeyword_bld <- get_data(all_data, "L244.FinalEnergyKeyword_bld_gcamchina")
    L244.SubsectorShrwtFllt_bld <- get_data(all_data, "L244.SubsectorShrwtFllt_bld_gcamchina")
    L244.SubsectorInterp_bld <- get_data(all_data, "L244.SubsectorInterp_bld_gcamchina")
    L244.SubsectorInterpTo_bld <- get_data(all_data, "L244.SubsectorInterpTo_bld_gcamchina")
    L244.SubsectorLogit_bld <- get_data(all_data, "L244.SubsectorLogit_bld_gcamchina")
    L244.StubTech_bld <- get_data(all_data, "L244.StubTech_bld_gcamchina")
    L244.StubTechCalInput_bld <- get_data(all_data, "L244.StubTechCalInput_bld_gcamchina")
    L244.StubTechMarket_bld <- get_data(all_data, "L244.StubTechMarket_bld_gcamchina")
    L244.GlobalTechIntGainOutputRatio<- get_data(all_data, "L244.GlobalTechIntGainOutputRatio_gcamchina")
    L244.GlobalTechInterpTo_bld <- get_data(all_data, "L244.GlobalTechInterpTo_bld_gcamchina")
    L244.GlobalTechEff_bld <- get_data(all_data, "L244.GlobalTechEff_bld_gcamchina")
    L244.GlobalTechShrwt_bld <- get_data(all_data, "L244.GlobalTechShrwt_bld_gcamchina")
    L244.GlobalTechCost_bld <- get_data(all_data, "L244.GlobalTechCost_bld_gcamchina")
    L244.GlobalTechSCurve_bld <- get_data(all_data, "L244.GlobalTechSCurve_bld_gcamchina")
    L244.GompFnParam_gcamchina <- get_data(all_data, "L244.GompFnParam_gcamchina")
    L244.Satiation_impedance <- get_data(all_data, "L244.Satiation_impedance_gcamchina")
    L244.GenericServiceImpedance <- get_data(all_data, "L244.GenericServiceImpedance_gcamchina")
    L244.GenericServiceCoef <- get_data(all_data, "L244.GenericServiceCoef_gcamchina")
    L244.GenericServiceAdder <- get_data(all_data, "L244.GenericServiceAdder_gcamchina")
    L244.ThermalServiceImpedance <- get_data(all_data, "L244.ThermalServiceImpedance_gcamchina")
    L244.ThermalServiceCoef <- get_data(all_data, "L244.ThermalServiceCoef_gcamchina")
    L244.ThermalServiceAdder <- get_data(all_data, "L244.ThermalServiceAdder_gcamchina")
    L244.GenericServicePrice_gcamchina <- get_data(all_data, "L244.GenericServicePrice_gcamchina")
    L244.ThermalServicePrice_gcamchina <- get_data(all_data, "L244.ThermalServicePrice_gcamchina")
    L244.GenericBaseDens_gcamchina <- get_data(all_data, "L244.GenericBaseDens_gcamchina")
    L244.ThermalBaseDens_gcamchina <- get_data(all_data, "L244.ThermalBaseDens_gcamchina")
    L244.DeleteThermalService_gcamchina <- get_data(all_data, "L244.DeleteThermalService_gcamchina")
    L244.DeleteGenericService_gcamchina <- get_data(all_data, "L244.DeleteGenericService_gcamchina")
    L244.FuelPrefElast_bld_gcamchina <- get_data(all_data,"L244.FuelPrefElast_bld_gcamchina")

    # ===================================================


    # Produce outputs
    create_xml("building_CHINA.xml") %>%
      add_xml_data(L244.DeleteConsumer_CHINAbld, "DeleteConsumer") %>%
      add_xml_data(L244.DeleteSupplysector_CHINAbld , "DeleteSupplysector") %>%
      add_xml_data(L244.SubregionalShares , "SubregionalShares") %>%
      add_xml_data(L244.PriceExp_IntGains, "PriceExp_IntGains") %>%
      add_xml_data(L244.Floorspace, "Floorspace") %>%
      add_xml_data(L244.DemandFunction_serv, "DemandFunction_serv") %>%
      add_xml_data(L244.DemandFunction_flsp, "DemandFunction_flsp") %>%
      add_xml_data(L244.Satiation_flsp, "Satiation_flsp") %>%
      add_xml_data(L244.Satiation_impedance, "SatiationImpedance") %>%
      add_xml_data(L244.SatiationAdder, "SatiationAdder") %>%
      add_xml_data(L244.GompFnParam_gcamchina, "GompFnParam")  %>%
      add_xml_data(L244.ThermalBaseService, "ThermalBaseService") %>%
      add_xml_data(L244.GenericBaseService, "GenericBaseService") %>%
      add_xml_data(L244.ThermalServiceSatiation, "ThermalServiceSatiation") %>%
      add_xml_data(L244.GenericServiceSatiation, "GenericServiceSatiation") %>%
      add_xml_data(L244.ThermalServiceImpedance, "ThermalServiceImpedance") %>%
      add_xml_data(L244.GenericServiceImpedance, "GenericServiceImpedance") %>%
      add_xml_data(L244.ThermalServiceAdder, "ThermalServiceAdder")  %>%
      add_xml_data(L244.GenericServiceAdder, "GenericServiceAdder") %>%
      add_xml_data(L244.ThermalServiceCoef, "ThermalServiceCoef") %>%
      add_xml_data(L244.GenericServiceCoef, "GenericServiceCoef") %>%
      add_xml_data(L244.Intgains_scalar, "Intgains_scalar") %>%
      add_xml_data(L244.ShellConductance_bld, "ShellConductance") %>%
      add_logit_tables_xml(L244.Supplysector_bld, "Supplysector") %>%
      add_xml_data(L244.FinalEnergyKeyword_bld, "FinalEnergyKeyword") %>%
      add_xml_data(L244.SubsectorShrwtFllt_bld, "SubsectorShrwtFllt") %>%
      add_xml_data(L244.SubsectorInterp_bld, "SubsectorInterp") %>%
      add_logit_tables_xml(L244.SubsectorLogit_bld, "SubsectorLogit") %>%
      add_xml_data(L244.StubTech_bld, "StubTech") %>%
      add_xml_data(L244.StubTechCalInput_bld, "StubTechCalInput") %>%
      add_xml_data(L244.StubTechMarket_bld, "StubTechMarket") %>%
      add_xml_data(L244.GlobalTechIntGainOutputRatio, "GlobalTechIntGainOutputRatio") %>%
      add_xml_data(L244.GlobalTechEff_bld, "GlobalTechEff") %>%
      add_xml_data(L244.GlobalTechShrwt_bld, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_bld, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechSCurve_bld, "GlobalTechSCurve")  %>%
      add_xml_data(L244.GenericServicePrice_gcamchina, "GenericServicePrice") %>%
      add_xml_data(L244.ThermalServicePrice_gcamchina, "ThermalServicePrice") %>%
      add_xml_data(L244.GenericBaseDens_gcamchina, "GenericBaseDens") %>%
      add_xml_data(L244.ThermalBaseDens_gcamchina, "ThermalBaseDens") %>%
      add_xml_data(L244.FuelPrefElast_bld_gcamchina, "FuelPrefElast")  %>%
      add_precursors("L244.DeleteConsumer_CHINAbld",
                     "L244.DeleteSupplysector_CHINAbld",
                     "L244.SubregionalShares_gcamchina",
                     "L244.PriceExp_IntGains_gcamchina",
                     "L244.Floorspace_gcamchina",
                     "L244.DemandFunction_serv_gcamchina",
                     "L244.DemandFunction_flsp_gcamchina",
                     "L244.Satiation_flsp_gcamchina",
                     "L244.SatiationAdder_gcamchina",
                     "L244.ThermalBaseService_gcamchina",
                     "L244.GenericBaseService_gcamchina",
                     "L244.ThermalServiceSatiation_gcamchina",
                     "L244.GenericServiceSatiation_gcamchina",
                     "L244.Intgains_scalar_gcamchina",
                     "L244.ShellConductance_bld_gcamchina",
                     "L244.Supplysector_bld_gcamchina",
                     "L244.FinalEnergyKeyword_bld_gcamchina",
                     "L244.SubsectorShrwtFllt_bld_gcamchina",
                     "L244.SubsectorInterp_bld_gcamchina",
                     "L244.SubsectorInterpTo_bld_gcamchina",
                     "L244.SubsectorLogit_bld_gcamchina",
                     "L244.StubTech_bld_gcamchina",
                     "L244.StubTechCalInput_bld_gcamchina",
                     "L244.StubTechMarket_bld_gcamchina",
                     "L244.GlobalTechIntGainOutputRatio_gcamchina",
                     "L244.GlobalTechEff_bld_gcamchina",
                     "L244.GlobalTechShrwt_bld_gcamchina",
                     "L244.GlobalTechCost_bld_gcamchina",
                     "L244.GlobalTechSCurve_bld_gcamchina",
                     "L244.GlobalTechInterpTo_bld_gcamchina",
                     "L244.GompFnParam_gcamchina",
                     "L244.Satiation_impedance_gcamchina",
                     "L244.GenericServiceImpedance_gcamchina",
                     "L244.GenericServiceCoef_gcamchina",
                     "L244.GenericServiceAdder_gcamchina",
                     "L244.ThermalServiceImpedance_gcamchina",
                     "L244.ThermalServiceCoef_gcamchina",
                     "L244.ThermalServiceAdder_gcamchina",
                     "L244.GenericServicePrice_gcamchina",
                     "L244.ThermalServicePrice_gcamchina",
                     "L244.GenericBaseDens_gcamchina",
                     "L244.ThermalBaseDens_gcamchina",
                     "L244.DeleteThermalService_gcamchina",
                     "L244.DeleteGenericService_gcamchina",
                     "L244.FuelPrefElast_bld_gcamchina") ->
      building_CHINA.xml

    # # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(nrow(L244.DeleteThermalService_gcamchina) > 0) {
      building_CHINA.xml %>%
        add_xml_data(L244.DeleteThermalService_gcamchina, "DeleteThermalService") ->
        building_CHINA.xml
    }

    if(!is.null(L244.DeleteGenericService_gcamchina)) {
      building_CHINA.xml %>%
        add_xml_data(L244.DeleteGenericService_gcamchina, "DeleteGenericService") ->
        building_CHINA.xml
    }

    if(!is.null(L244.SubsectorInterpTo_bld)) {

      building_CHINA.xml %>%
        add_xml_data(L244.SubsectorInterpTo_bld, "SubsectorInterpTo") ->
        building_CHINA.xml
    }

    if(!is.null(L244.GlobalTechInterpTo_bld)) {

      building_CHINA.xml %>%
        add_xml_data(L244.GlobalTechInterpTo_bld, "GlobalTechInterpTo") ->
        building_CHINA.xml
    }


    return_data(building_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
