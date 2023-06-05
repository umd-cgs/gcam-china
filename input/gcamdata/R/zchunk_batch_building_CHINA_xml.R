# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcam.china_batch_building_CHINA_xml
#'
#' Construct XML data structure for \code{building_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{building_CHINA.xml}. The corresponding file in the
#' original data system was \code{batch_building_CHINA.xml} (gcamusa XML).
module_gcam.china_batch_building_CHINA_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L244.DeleteConsumer_CHINAbld",
             "L244.DeleteSupplysector_CHINAbld",
             "L244.SubregionalShares_CHINAbld",
             "L244.PriceExp_IntGains_CHINAbld",
             "L244.Floorspace_CHINAbld",
             "L244.DemandFunction_serv_CHINAbld",
             "L244.DemandFunction_flsp_CHINAbld",
             "L244.Satiation_flsp_CHINAbld",
             "L244.SatiationAdder_CHINAbld",
             "L244.ThermalBaseService_CHINAbld",
             "L244.GenericBaseService_CHINAbld",
             "L244.ThermalServiceSatiation_CHINAbld",
             "L244.GenericServiceSatiation_CHINAbld",
             "L244.Intgains_scalar_CHINAbld",
             "L244.ShellConductance_CHINAbld",
             "L244.Supplysector_CHINAbld",
             "L244.FinalEnergyKeyword_CHINAbld",
             "L244.SubsectorShrwtFllt_CHINAbld",
             "L244.SubsectorInterp_CHINAbld",
             "L244.SubsectorInterpTo_CHINAbld",
             "L244.SubsectorLogit_CHINAbld",
             "L244.StubTech_CHINAbld",
             "L244.StubTechCalInput_CHINAbld",
             "L244.StubTechMarket_CHINAbld",
             "L244.GlobalTechIntGainOutputRatio_CHINAbld",
             "L244.GlobalTechInterpTo_CHINAbld",
             "L244.GlobalTechEff_CHINAbld",
             "L244.GlobalTechShrwt_CHINAbld",
             "L244.GlobalTechCost_CHINAbld",
             "L244.GlobalTechSCurve_CHINAbld",
             "L244.FuelPrefElast_CHINAbld"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "building_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.DeleteConsumer_CHINAbld <- get_data(all_data, "L244.DeleteConsumer_CHINAbld")
    L244.DeleteSupplysector_CHINAbld <- get_data(all_data, "L244.DeleteSupplysector_CHINAbld")
    L244.SubregionalShares_CHINAbld <- get_data(all_data, "L244.SubregionalShares_CHINAbld")
    L244.PriceExp_IntGains_CHINAbld <- get_data(all_data, "L244.PriceExp_IntGains_CHINAbld")
    L244.Floorspace_CHINAbld <- get_data(all_data, "L244.Floorspace_CHINAbld")
    L244.DemandFunction_serv_CHINAbld <- get_data(all_data, "L244.DemandFunction_serv_CHINAbld")
    L244.DemandFunction_flsp_CHINAbld <- get_data(all_data, "L244.DemandFunction_flsp_CHINAbld")
    L244.Satiation_flsp_CHINAbld <- get_data(all_data, "L244.Satiation_flsp_CHINAbld")
    L244.SatiationAdder_CHINAbld <- get_data(all_data, "L244.SatiationAdder_CHINAbld")
    L244.ThermalBaseService_CHINAbld <- get_data(all_data, "L244.ThermalBaseService_CHINAbld")
    L244.GenericBaseService_CHINAbld <- get_data(all_data, "L244.GenericBaseService_CHINAbld")
    L244.ThermalServiceSatiation_CHINAbld <- get_data(all_data, "L244.ThermalServiceSatiation_CHINAbld")
    L244.GenericServiceSatiation_CHINAbld <- get_data(all_data, "L244.GenericServiceSatiation_CHINAbld")
    L244.Intgains_scalar_CHINAbld <- get_data(all_data, "L244.Intgains_scalar_CHINAbld")
    L244.ShellConductance_CHINAbld <- get_data(all_data, "L244.ShellConductance_CHINAbld")
    L244.Supplysector_CHINAbld <- get_data(all_data, "L244.Supplysector_CHINAbld")
    L244.FinalEnergyKeyword_CHINAbld <- get_data(all_data, "L244.FinalEnergyKeyword_CHINAbld")
    L244.SubsectorShrwtFllt_CHINAbld <- get_data(all_data, "L244.SubsectorShrwtFllt_CHINAbld")
    L244.SubsectorInterp_CHINAbld <- get_data(all_data, "L244.SubsectorInterp_CHINAbld")
    L244.SubsectorInterpTo_CHINAbld <- get_data(all_data, "L244.SubsectorInterpTo_CHINAbld")
    L244.SubsectorLogit_CHINAbld <- get_data(all_data, "L244.SubsectorLogit_CHINAbld")
    L244.StubTech_CHINAbld <- get_data(all_data, "L244.StubTech_CHINAbld")
    L244.StubTechCalInput_CHINAbld <- get_data(all_data, "L244.StubTechCalInput_CHINAbld")
    L244.StubTechMarket_CHINAbld <- get_data(all_data, "L244.StubTechMarket_CHINAbld")
    L244.GlobalTechIntGainOutputRatio_CHINAbld <- get_data(all_data, "L244.GlobalTechIntGainOutputRatio_CHINAbld")
    L244.GlobalTechInterpTo_CHINAbld <- get_data(all_data, "L244.GlobalTechInterpTo_CHINAbld")
    L244.GlobalTechEff_CHINAbld <- get_data(all_data, "L244.GlobalTechEff_CHINAbld")
    L244.GlobalTechShrwt_CHINAbld <- get_data(all_data, "L244.GlobalTechShrwt_CHINAbld")
    L244.GlobalTechCost_CHINAbld <- get_data(all_data, "L244.GlobalTechCost_CHINAbld")
    L244.GlobalTechSCurve_CHINAbld <- get_data(all_data, "L244.GlobalTechSCurve_CHINAbld")
    L244.FuelPrefElast_CHINAbld <- get_data(all_data, "L244.FuelPrefElast_CHINAbld")

    # ===================================================

    # Produce outputs
    create_xml("building_CHINA.xml") %>%
      add_xml_data(L244.DeleteConsumer_CHINAbld, "DeleteConsumer") %>%
      add_xml_data(L244.DeleteSupplysector_CHINAbld, "DeleteSupplysector") %>%
      add_xml_data(L244.SubregionalShares_CHINAbld, "SubregionalShares") %>%
      add_xml_data(L244.PriceExp_IntGains_CHINAbld, "PriceExp_IntGains") %>%
      add_xml_data(L244.Floorspace_CHINAbld, "Floorspace") %>%
      add_xml_data(L244.DemandFunction_serv_CHINAbld, "DemandFunction_serv") %>%
      add_xml_data(L244.DemandFunction_flsp_CHINAbld, "DemandFunction_flsp") %>%
      add_xml_data(L244.Satiation_flsp_CHINAbld, "Satiation_flsp") %>%
      add_xml_data(L244.SatiationAdder_CHINAbld, "SatiationAdder") %>%
      add_xml_data(L244.ThermalBaseService_CHINAbld, "ThermalBaseService") %>%
      add_xml_data(L244.GenericBaseService_CHINAbld, "GenericBaseService") %>%
      add_xml_data(L244.ThermalServiceSatiation_CHINAbld, "ThermalServiceSatiation") %>%
      add_xml_data(L244.GenericServiceSatiation_CHINAbld, "GenericServiceSatiation") %>%
      add_xml_data(L244.Intgains_scalar_CHINAbld, "Intgains_scalar") %>%
      add_xml_data(L244.ShellConductance_CHINAbld, "ShellConductance") %>%
      add_logit_tables_xml(L244.Supplysector_CHINAbld, "Supplysector") %>%
      add_xml_data(L244.FinalEnergyKeyword_CHINAbld, "FinalEnergyKeyword") %>%
      add_xml_data(L244.SubsectorShrwtFllt_CHINAbld, "SubsectorShrwtFllt") %>%
      add_xml_data(L244.SubsectorInterp_CHINAbld, "SubsectorInterp") %>%
      add_logit_tables_xml(L244.SubsectorLogit_CHINAbld, "SubsectorLogit") %>%
      add_xml_data(L244.StubTech_CHINAbld, "StubTech") %>%
      add_xml_data(L244.StubTechCalInput_CHINAbld, "StubTechCalInput") %>%
      add_xml_data(L244.StubTechMarket_CHINAbld, "StubTechMarket") %>%
      add_xml_data(L244.GlobalTechIntGainOutputRatio_CHINAbld, "GlobalTechIntGainOutputRatio") %>%
      add_xml_data(L244.GlobalTechInterpTo_CHINAbld, "GlobalTechInterpTo") %>%
      add_xml_data(L244.GlobalTechEff_CHINAbld, "GlobalTechEff") %>%
      add_xml_data(L244.GlobalTechShrwt_CHINAbld, "GlobalTechShrwt") %>%
      add_xml_data(L244.GlobalTechCost_CHINAbld, "GlobalTechCost") %>%
      add_xml_data(L244.GlobalTechSCurve_CHINAbld, "GlobalTechSCurve")  %>%
      add_xml_data(L244.FuelPrefElast_CHINAbld, "FuelPrefElast")  %>%
      add_precursors("L244.DeleteConsumer_CHINAbld",
                     "L244.DeleteSupplysector_CHINAbld",
                     "L244.SubregionalShares_CHINAbld",
                     "L244.PriceExp_IntGains_CHINAbld",
                     "L244.Floorspace_CHINAbld",
                     "L244.DemandFunction_serv_CHINAbld",
                     "L244.DemandFunction_flsp_CHINAbld",
                     "L244.Satiation_flsp_CHINAbld",
                     "L244.SatiationAdder_CHINAbld",
                     "L244.ThermalBaseService_CHINAbld",
                     "L244.GenericBaseService_CHINAbld",
                     "L244.ThermalServiceSatiation_CHINAbld",
                     "L244.GenericServiceSatiation_CHINAbld",
                     "L244.Intgains_scalar_CHINAbld",
                     "L244.ShellConductance_CHINAbld",
                     "L244.Supplysector_CHINAbld",
                     "L244.FinalEnergyKeyword_CHINAbld",
                     "L244.SubsectorShrwtFllt_CHINAbld",
                     "L244.SubsectorInterp_CHINAbld",
                     "L244.SubsectorInterpTo_CHINAbld",
                     "L244.SubsectorLogit_CHINAbld",
                     "L244.StubTech_CHINAbld",
                     "L244.StubTechCalInput_CHINAbld",
                     "L244.StubTechMarket_CHINAbld",
                     "L244.GlobalTechIntGainOutputRatio_CHINAbld",
                     "L244.GlobalTechInterpTo_CHINAbld",
                     "L244.GlobalTechEff_CHINAbld",
                     "L244.GlobalTechShrwt_CHINAbld",
                     "L244.GlobalTechCost_CHINAbld",
                     "L244.GlobalTechSCurve_CHINAbld",
                     "L244.FuelPrefElast_CHINAbld") ->
      building_CHINA.xml

    # # Some data inputs may not actually contain data. If so, do not add_xml_data.
    if(!is.null(L244.SubsectorInterpTo_CHINAbld)) {

      building_CHINA.xml %>%
        add_xml_data(L244.SubsectorInterpTo_CHINAbld, "SubsectorInterpTo") ->
        building_CHINA.xml

      }

    return_data(building_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
