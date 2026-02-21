# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_coal_vintage_china_xml

#' Construct XML data structure for \code{coal_vintage_CHINA.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{coal_vintage_CHINA.xml}.
module_energy_coal_vintage_china_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2231.StubTechProd_coal_vintage_CHINA",
             "L2231.StubTechEff_coal_vintage_CHINA",
             "L2231.StubTechMarket_coal_vintage_CHINA",
             "L2231.GlobalTechEff_coal_vintage_CHINA",
             "L2231.GlobalTechCapFac_coal_vintage_CHINA",
             "L2231.GlobalTechCapital_coal_vintage_CHINA",
             "L2231.GlobalTechOMfixed_coal_vintage_CHINA",
             "L2231.GlobalTechOMvar_coal_vintage_CHINA",
             "L2231.GlobalTechShrwt_coal_vintage_CHINA",
             "L2231.GlobalTechProfitShutdown_coal_vintage_CHINA",
             "L2231.GlobalTechSCurve_coal_vintage_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "coal_vintage_CHINA.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # silence package check notes
    tech.share.weight <- share.weight <- sector.name <- supplysector <- subsector.name <- subsector <- NULL

    # Load required inputs
    # coal vintage
    L2231.StubTechProd_coal_vintage_CHINA <- get_data(all_data, "L2231.StubTechProd_coal_vintage_CHINA")
    L2231.StubTechEff_coal_vintage_CHINA <- get_data(all_data, "L2231.StubTechEff_coal_vintage_CHINA")
    L2231.StubTechMarket_coal_vintage_CHINA <- get_data(all_data, "L2231.StubTechMarket_coal_vintage_CHINA")
    L2231.GlobalTechEff_coal_vintage_CHINA <- get_data(all_data, "L2231.GlobalTechEff_coal_vintage_CHINA")
    L2231.GlobalTechCapFac_coal_vintage_CHINA <- get_data(all_data, "L2231.GlobalTechCapFac_coal_vintage_CHINA")
    L2231.GlobalTechCapital_coal_vintage_CHINA <- get_data(all_data, "L2231.GlobalTechCapital_coal_vintage_CHINA")
    L2231.GlobalTechOMfixed_coal_vintage_CHINA <- get_data(all_data, "L2231.GlobalTechOMfixed_coal_vintage_CHINA")
    L2231.GlobalTechOMvar_coal_vintage_CHINA <- get_data(all_data, "L2231.GlobalTechOMvar_coal_vintage_CHINA")
    L2231.GlobalTechShrwt_coal_vintage_CHINA <- get_data(all_data, "L2231.GlobalTechShrwt_coal_vintage_CHINA")
    L2231.GlobalTechProfitShutdown_coal_vintage_CHINA <- get_data(all_data, "L2231.GlobalTechProfitShutdown_coal_vintage_CHINA")
    L2231.GlobalTechSCurve_coal_vintage_CHINA <- get_data(all_data, "L2231.GlobalTechSCurve_coal_vintage_CHINA")

    # Produce outputs
    create_xml("coal_vintage_CHINA.xml") %>%
      add_xml_data(L2231.StubTechProd_coal_vintage_CHINA, "StubTechProd") %>%
      add_xml_data(L2231.StubTechEff_coal_vintage_CHINA, "StubTechEff") %>%
      ##add_xml_data(L2241.StubTechSCurve_coal_vintage_USA, "StubTechSCurve") %>%
      ##add_xml_data(L2241.StubTechProfitShutdown_coal_vintage_USA, "StubTechProfitShutdown") %>%
      add_xml_data(L2231.StubTechMarket_coal_vintage_CHINA, "StubTechMarket") %>%
      add_xml_data(L2231.GlobalTechShrwt_coal_vintage_CHINA, "GlobalTechShrwt") %>%
      add_xml_data(L2231.GlobalTechEff_coal_vintage_CHINA, "GlobalTechEff") %>%
      add_xml_data(L2231.GlobalTechCapFac_coal_vintage_CHINA, "GlobalTechCapFac") %>%
      add_xml_data(L2231.GlobalTechCapital_coal_vintage_CHINA, "GlobalTechCapital") %>%
      add_xml_data(L2231.GlobalTechOMfixed_coal_vintage_CHINA, "GlobalTechOMfixed") %>%
      add_xml_data(L2231.GlobalTechOMvar_coal_vintage_CHINA, "GlobalTechOMvar") %>%
      add_xml_data(L2231.GlobalTechProfitShutdown_coal_vintage_CHINA, "GlobalTechProfitShutdown") %>% ##added
      add_xml_data(L2231.GlobalTechSCurve_coal_vintage_CHINA, "GlobalTechSCurve") %>% ##added
      add_precursors("L2231.StubTechProd_coal_vintage_CHINA",
                     "L2231.StubTechEff_coal_vintage_CHINA",
                     "L2231.StubTechMarket_coal_vintage_CHINA",
                     "L2231.GlobalTechEff_coal_vintage_CHINA",
                     "L2231.GlobalTechCapFac_coal_vintage_CHINA",
                     "L2231.GlobalTechCapital_coal_vintage_CHINA",
                     "L2231.GlobalTechOMfixed_coal_vintage_CHINA",
                     "L2231.GlobalTechOMvar_coal_vintage_CHINA",
                     "L2231.GlobalTechShrwt_coal_vintage_CHINA",
                     "L2231.GlobalTechProfitShutdown_coal_vintage_CHINA",
                     "L2231.GlobalTechSCurve_coal_vintage_CHINA") ->
      coal_vintage_CHINA.xml

    return_data(coal_vintage_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
