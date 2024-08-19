# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L244.building_CHINA
#'
#' Creates GCAM-CHINA building output files for writing to xml.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L244.DeleteConsumer_CHINAbld}, \code{L244.DeleteSupplysector_CHINAbld}, \code{L244.SubregionalShares_CHINAbld},
#' \code{L244.PriceExp_IntGains_CHINAbld}, \code{L244.Floorspace_CHINAbld}, \code{L244.DemandFunction_serv_CHINAbld}, \code{L244.DemandFunction_flsp_CHINAbld},
#' \code{L244.Satiation_flsp_CHINAbld}, \code{L244.SatiationAdder_CHINAbld}, \code{L244.ThermalBaseService_CHINAbld}, \code{L244.GenericBaseService_CHINAbld},
#' \code{L244.ThermalServiceSatiation_CHINAbld}, \code{L244.GenericServiceSatiation_CHINAbld}, \code{L244.Intgains_scalar_CHINAbld},
#' \code{L244.ShellConductance_CHINAbld}, \code{L244.Supplysector_CHINAbld}, \code{L244.FinalEnergyKeyword_CHINAbld}, \code{L244.SubsectorShrwt_CHINAbld},
#' \code{L244.SubsectorShrwtFllt_CHINAbld}, \code{L244.SubsectorInterp_CHINAbld}, \code{L244.SubsectorInterpTo_CHINAbld},
#' \code{L244.SubsectorLogit_CHINAbld}, \code{L244.StubTech_CHINAbld}, \code{L244.StubTechCalInput_CHINAbld}, \code{L244.StubTechMarket_CHINAbld},
#' \code{L244.GlobalTechIntGainOutputRatio_CHINAbld}, \code{L244.GlobalTechInterpTo_CHINAbld}, \code{L244.GlobalTechEff_CHINAbld},
#' \code{L244.GlobalTechShrwt_CHINAbld}, \code{L244.GlobalTechCost_CHINAbld}, \code{L244.GlobalTechSCurve_CHINAbld},
#' \code{L244.FuelPrefElast_CHINAbld}.
#' The corresponding file in the original data system was \code{L244.building_CHINA.R} (gcam-china level2).
#' @details Creates GCAM-CHINA building output files for writing to xml.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join mutate select semi_join summarise
#' @importFrom tidyr gather spread
#' @author BY January 2020

module_gcamchina_L244.building_CHINA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A44.gcam_consumer",
             FILE = "energy/A44.sector",
             FILE = "gcam-china/calibrated_techs_bld_china",
             FILE = "gcam-china/province_names_mappings",
             FILE = "gcam-china/A44.bld_shell_conductance",
             FILE = "gcam-china/A44.demandFn_flsp",
             FILE = "gcam-china/A44.demandFn_serv",
             FILE = "gcam-china/A44.gcam_consumer",
             FILE = "gcam-china/A44.satiation_flsp",
             FILE = "gcam-china/A44.sector",
             FILE = "gcam-china/A44.subsector_interp",
             FILE = "gcam-china/A44.subsector_logit",
             FILE = "gcam-china/A44.subsector_shrwt",
             FILE = "gcam-china/A44.globaltech_cost",
             FILE = "gcam-china/A44.globaltech_eff",
             FILE = "gcam-china/A44.globaltech_intgains",
             FILE = "gcam-china/A44.globaltech_retirement",
             FILE = "gcam-china/A44.globaltech_shrwt",
             FILE = "gcam-china/A44.globaltech_interp",
             FILE = "gcam-china/A44.demand_satiation_mult",
             FILE = "gcam-china/A44.fuelprefElasticity",
             "L144.flsp_bm2_province_bld",
             "L144.in_EJ_province_bld_F_U",
             "L101.Pop_thous_province",
             "L101.pcGDP_thous90usd_province"))
  } else if(command == driver.DECLARE_OUTPUTS) {
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
             "L244.SubsectorShrwt_CHINAbld",
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
  } else if(command == driver.MAKE) {

    # Silence package checks
    value <- province <- sector <- base.building.size <- year <- gcam.consumer <- resid_urban <- resid_rural <-
      comm <- pop <- pcflsp_mm2cap <- satiation.level <- pcGDP <- pcFlsp_mm2 <- satiation.adder <- . <- year.fillout <-
      to.value <- supplysector <- subsector <- technology <- minicam.energy.input <- region <- market.name <-
      calibrated.value <- calOutputValue <- share.weight <- input.cost <- sector.name <- steepness_stock <- steepness_new <-
      half_life_stock <- half_life_new <- input.ratio <- efficiency <- internal.gains.output.ratio <-
      internal.gains.market.name <- base.service <- multiplier <- thermal.building.service.input <-
      degree.days <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A44.gcam_consumer_en <- get_data(all_data, "energy/A44.gcam_consumer", strip_attributes = T)
    A44.sector_en <- get_data(all_data, "energy/A44.sector", strip_attributes = T)
    calibrated_techs_bld_china <- get_data(all_data, "gcam-china/calibrated_techs_bld_china", strip_attributes = T)
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = T)
    A44.bld_shell_conductance <- get_data(all_data, "gcam-china/A44.bld_shell_conductance", strip_attributes = T)
    A44.demandFn_flsp <- get_data(all_data, "gcam-china/A44.demandFn_flsp", strip_attributes = T)
    A44.demandFn_serv <- get_data(all_data, "gcam-china/A44.demandFn_serv", strip_attributes = T)
    A44.gcam_consumer <- get_data(all_data, "gcam-china/A44.gcam_consumer", strip_attributes = T)
    A44.satiation_flsp <- get_data(all_data, "gcam-china/A44.satiation_flsp", strip_attributes = T)
    A44.sector <- get_data(all_data, "gcam-china/A44.sector", strip_attributes = T)
    A44.subsector_interp <- get_data(all_data, "gcam-china/A44.subsector_interp", strip_attributes = T)
    A44.subsector_logit <- get_data(all_data, "gcam-china/A44.subsector_logit", strip_attributes = T)
    A44.subsector_shrwt <- get_data(all_data, "gcam-china/A44.subsector_shrwt", strip_attributes = T)
    A44.globaltech_cost <- get_data(all_data, "gcam-china/A44.globaltech_cost", strip_attributes = T)
    A44.globaltech_eff <- get_data(all_data, "gcam-china/A44.globaltech_eff", strip_attributes = T) %>%
      gather_years()
    A44.globaltech_intgains <- get_data(all_data, "gcam-china/A44.globaltech_intgains", strip_attributes = T)
    A44.globaltech_retirement <- get_data(all_data, "gcam-china/A44.globaltech_retirement", strip_attributes = T)
    A44.globaltech_shrwt <- get_data(all_data, "gcam-china/A44.globaltech_shrwt", strip_attributes = T)
    A44.globaltech_interp <- get_data(all_data, "gcam-china/A44.globaltech_interp", strip_attributes = T)
    A44.demand_satiation_mult <- get_data(all_data, "gcam-china/A44.demand_satiation_mult", strip_attributes = T)
    A44.fuelprefElasticity <- get_data(all_data, "gcam-china/A44.fuelprefElasticity", strip_attributes = T)
    L144.flsp_bm2_province_bld <- get_data(all_data, "L144.flsp_bm2_province_bld", strip_attributes = T)
    L144.in_EJ_province_bld_F_U <- get_data(all_data, "L144.in_EJ_province_bld_F_U", strip_attributes = T)
    L101.Pop_thous_province <- get_data(all_data, "L101.Pop_thous_province", strip_attributes = T)
    L101.pcGDP_thous90usd_province <- get_data(all_data, "L101.pcGDP_thous90usd_province", strip_attributes = T)

    # ===================================================
    # Data Processing

    # Need to delete the buildings sector in the CHINA region (gcam.consumers and supplysectors)
    L244.DeleteConsumer_CHINAbld <- tibble(region = gcamchina.REGION, gcam.consumer = A44.gcam_consumer_en$gcam.consumer)
    L244.DeleteSupplysector_CHINAbld <- tibble(region = gcamchina.REGION, supplysector = A44.sector_en$supplysector)

    #Subregional population and income shares: need to be read in because these default to 0
    # L244.SubregionalShares_CHINAbld: subregional population and income shares (not currently used)
    #TODO: fix subregional shares
    L244.SubregionalShares_CHINAbld <- write_to_all_provinces(A44.gcam_consumer, c("region", "gcam.consumer"), gcamchina.PROVINCES_ALL) %>%
      mutate(pop.year.fillout = min(MODEL_BASE_YEARS),
             inc.year.fillout = min(MODEL_BASE_YEARS),
             subregional.population.share = 1,
             subregional.income.share = 1)

    # L244.PriceExp_IntGains_CHINAbld: price exponent on floorspace and naming of internal gains trial markets
    L244.PriceExp_IntGains_CHINAbld <- write_to_all_provinces(A44.gcam_consumer, LEVEL2_DATA_NAMES[["PriceExp_IntGains"]], gcamchina.PROVINCES_ALL)

    # L244.Floorspace_CHINAbld: base year floorspace
    # Keep all historical years for now - these are needed in calculating satiation adders later on

    L244.bld_nodes_noregion <- A44.gcam_consumer %>%
      select(c("gcam.consumer", "nodeInput", "building.node.input"))

    L244.Floorspace_full<- L144.flsp_bm2_province_bld %>%
      mutate(base.building.size = round(value, gcamchina.DIGITS_FLOORSPACE)) %>%
      rename(region = province,
             gcam.consumer = sector) %>%
      na.omit() %>%
      left_join_error_no_match(L244.bld_nodes_noregion, by = c("gcam.consumer")) %>%
      select(LEVEL2_DATA_NAMES[["Floorspace"]]) %>%
      #TODO: fix floorspace
      mutate(base.building.size = pmax(base.building.size, gcamchina.MIN_BASE_BUILDING_SIZE))

    L244.Floorspace_CHINAbld <- subset( L244.Floorspace_full, year %in% MODEL_BASE_YEARS )

    # L244.DemandFunction_serv_CHINAbld and L244.DemandFunction_flsp_CHINAbld: demand function types
    L244.DemandFunction_serv_CHINAbld <- write_to_all_provinces(A44.demandFn_serv, LEVEL2_DATA_NAMES[["DemandFunction_serv"]], gcamchina.PROVINCES_ALL)
    L244.DemandFunction_flsp_CHINAbld <- write_to_all_provinces(A44.demandFn_flsp, LEVEL2_DATA_NAMES[["DemandFunction_flsp"]], gcamchina.PROVINCES_ALL)

    # L244.Satiation_flsp_CHINAbld: Satiation levels assumed for floorspace
    L244.Satiation_flsp_CHINAbld <- A44.satiation_flsp %>%
      gather(key = gcam.consumer, value = value, c(resid_urban, resid_rural, comm)) %>%
      rename(region = province) %>%
      # Need to make sure that the satiation level is greater than the floorspace in the final base year
      # Use left join as HK and MC are present in the floorspace table, remove these
      left_join(L244.Floorspace_CHINAbld %>%
                                 filter(year == max(MODEL_BASE_YEARS)), by = c("region", "gcam.consumer")) %>%
      na.omit() %>%
      left_join_error_no_match(L101.Pop_thous_province, by = c("region" = "province", "year")) %>%
      mutate(year = as.integer(year),
             pcflsp_mm2cap = base.building.size / pop,
             # Satiation level = must be greater than the observed value in the final calibration year, so if observed value is
             # greater than calculated, multiply observed by 1.001
             satiation.level = round(pmax(value * CONV_THOUS_BIL, pcflsp_mm2cap * 1.001), energy.DIGITS_SATIATION_ADDER)) %>%
      left_join_error_no_match(A44.gcam_consumer, by = c("gcam.consumer", "nodeInput", "building.node.input")) %>%
      select(LEVEL2_DATA_NAMES[["BldNodes"]], "satiation.level")

    # L244.SatiationAdder_CHINAbld: Satiation adders in floorspace demand function
    # Satiation adder - this is total BS. Required for shaping the future floorspace growth trajectories in each region

    # We will filter GDP to energy.SATIATION_YEAR, but this may be greater than the historical years present
    # under timeshift conditions. So we adjust energy.SATIATION_YEAR
    energy.SATIATION_YEAR <- min(max(MODEL_BASE_YEARS), energy.SATIATION_YEAR)

    # Match in the per-capita GDP, total floorspace, and population (for calculating per-capita floorspace)
    L244.SatiationAdder_CHINAbld <- L244.Satiation_flsp_CHINAbld %>%
      # Add per capita GDP
      left_join_error_no_match(L101.pcGDP_thous90usd_province %>%
                                 filter(year == energy.SATIATION_YEAR), by = c("region" = "province")) %>%
      # Add floorspace
      left_join_error_no_match(L244.Floorspace_full, by = c("region", "gcam.consumer", "year", "nodeInput", "building.node.input")) %>%
      # Add population
      left_join_error_no_match(L101.Pop_thous_province, by = c("region" = "province", "year")) %>%
      # Calculate per capita floorspace
      mutate(pcFlsp_mm2 = base.building.size / pop,
             # Calculate the satiation adders
             satiation.adder = round(satiation.level - (
               exp(log(2) * pcGDP / energy.GDP_MID_SATIATION) * (satiation.level - pcFlsp_mm2)),
               energy.DIGITS_SATIATION_ADDER),
             # The satiation adder (million square meters of floorspace per person) needs to be less than the per-capita demand in the final calibration year
             satiation.adder = if_else(satiation.adder > pcFlsp_mm2, pcFlsp_mm2 * 0.999, satiation.adder)) %>%
      #NOTE: satiation adder is very slightly different (in hundreds decimal place) from old data system, because of slight changes in pcGDP
      select(LEVEL2_DATA_NAMES[["SatiationAdder"]])

    # Heating and cooling degree days (thermal services only)
    # First, separate the thermal from the generic services. Generic services will be assumed to produce
    # internal gain energy, so anything in the internal gains assumptions table will be assumed generic
    generic_services <- unique(A44.globaltech_intgains$supplysector)
    thermal_services <- dplyr::setdiff(unique(A44.sector$supplysector), generic_services)

    # L244.ShellConductance_CHINAbld: Shell conductance (inverse of shell efficiency)
    L244.ShellConductance_CHINAbld  <- A44.bld_shell_conductance %>%
      # Convert to long form
      gather_years()  %>%
      # Interpolate to model years
      complete(gcam.consumer, year = c(year, MODEL_YEARS)) %>%
      group_by(gcam.consumer) %>%
      mutate(value = round(approx_fun(year, value), energy.DIGITS_EFFICIENCY)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      # Repeat for all provinces
      write_to_all_provinces(names = c(names(.), "region"), gcamchina.PROVINCES_ALL) %>%
      # Add nodeInput and building.node.input
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer") %>%
      mutate(floor.to.surface.ratio = energy.FLOOR_TO_SURFACE_RATIO,
             shell.year = year) %>%
      # Rename columns
      rename(shell.conductance = value) %>%
      select(LEVEL2_DATA_NAMES[["ShellConductance"]])

    # The remainder of the building-level parameters require information about the output of each service, which we do not have yet
    # First, technology-level inputs and efficiencies need to be assigned.
    # So moving to service supply sectors/subsectors/technologies

    # L244.Supplysector_CHINAbld: Supplysector info for buildings
    L244.Supplysector_CHINAbld <- write_to_all_provinces(A44.sector, c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), gcamchina.PROVINCES_ALL)


    # L244.FinalEnergyKeyword_CHINAbld: Supply sector keywords for detailed building sector
    L244.FinalEnergyKeyword_CHINAbld <- write_to_all_provinces(A44.sector, LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]], gcamchina.PROVINCES_ALL)

    # L244.SubsectorLogit_CHINAbld: Subsector logit exponents of building sector
    L244.SubsectorLogit_CHINAbld <- write_to_all_provinces(A44.subsector_logit, c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), gcamchina.PROVINCES_ALL)

    # L244.SubsectorShrwt_CHINAbld and L244.SubsectorShrwtFllt_CHINAbld: Subsector shareweights of building sector
    if(any(!is.na(A44.subsector_shrwt$year))) {
      L244.SubsectorShrwt_CHINAbld <- write_to_all_provinces(A44.subsector_shrwt %>%
                                                               filter(!is.na(year)), LEVEL2_DATA_NAMES[["SubsectorShrwt"]], gcamchina.PROVINCES_ALL)
    }
    if(any(!is.na(A44.subsector_shrwt$year.fillout))) {
      L244.SubsectorShrwtFllt_CHINAbld <- write_to_all_provinces(A44.subsector_shrwt %>%
                                                                   filter(!is.na(year.fillout)), LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], gcamchina.PROVINCES_ALL)
    }

    # L244.SubsectorInterp_CHINAbld and L244.SubsectorInterpTo_CHINAbld: Subsector shareweight interpolation of building sector
    if(any(is.na(A44.subsector_interp$to.value))) {
      L244.SubsectorInterp_CHINAbld <- write_to_all_provinces(A44.subsector_interp %>%
                                                                filter(is.na(to.value)), LEVEL2_DATA_NAMES[["SubsectorInterp"]], gcamchina.PROVINCES_ALL)
    }
    if(any(!is.na(A44.subsector_interp$to.value))) {
      L244.SubsectorInterpTo_CHINAbld <- write_to_all_provinces(A44.subsector_interp %>%
                                                                  filter(!is.na(to.value)), LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], gcamchina.PROVINCES_ALL)
    }

    # L244.StubTech_CHINAbld Identification of stub technologies for buildings
    L244.StubTech_CHINAbld <- A44.globaltech_eff %>%
      select(supplysector, subsector, technology) %>%
      distinct() %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["Tech"]], gcamchina.PROVINCES_ALL) %>%
      rename(stub.technology = technology)

    # L244.GlobalTechEff_CHINAbld: Assumed efficiencies (all years) of buildings technologies
    L244.end_use_eff <- A44.globaltech_eff %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, MODEL_YEARS)) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(value = approx_fun(year, value)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      rename(efficiency = value)

    # Note - this code assumes that base-year efficiences are identical (should fix to copy over to make sure)
    L244.GlobalTechEff_CHINAbld <- L244.end_use_eff %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]])

    # L244.StubTechMarket_CHINAbld: Specify market names for fuel inputs to all technologies in each province
    L244.StubTechMarket_CHINAbld <- L244.end_use_eff %>%
      mutate(market.name = gcamchina.REGION) %>%
      #mutate(market.name = region) %>%
      rename(stub.technology = technology) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["StubTechMarket"]], gcamchina.PROVINCES_ALL) %>%
      # Electricity is consumed from province markets, so change market.name to provinces for electricity
      mutate(market.name = if_else(minicam.energy.input %in% gcamchina.ELECT_TD_SECTORS, region, market.name)) %>%
      # replace market name with the province name if the minicam.energy.input is
      # considered a regional fuel market
      left_join_error_no_match(province_names_mappings, by = c("region" = "province")) %>%
      mutate(market.name = if_else(minicam.energy.input %in% gcamchina.REGIONAL_FUEL_MARKETS,
                                     region, market.name)) %>%
      mutate(market.name = if_else(minicam.energy.input == "district heat",
                                   region, market.name)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechMarket"]])

    # L244.StubTechCalInput_CHINAbld: Calibrated energy consumption by buildings technologies
    L244.in_EJ_R_bld_serv_F_Yh <- L144.in_EJ_province_bld_F_U %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      # Add subsector and energy.input
      # must use left_join as there are NAs present, these will be filtered out
      left_join(calibrated_techs_bld_china, by = c("sector", "fuel", "service")) %>%
      na.omit(supplysector) %>%
      rename(region = province,
             stub.technology = technology)

    #TODO: Do not have tech partitions
    L244.StubTechCalInput_CHINAbld <- L244.in_EJ_R_bld_serv_F_Yh

    #Ensure that technologies that may have existed by did not have any base year data gets a
    #zero share-weight
    L244.StubTechCalInput_CHINAbld.nocal <- L244.StubTechMarket_CHINAbld %>%
      anti_join(L244.StubTechCalInput_CHINAbld, by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
      filter(year <= max(HISTORICAL_YEARS)) %>%
      mutate(calibrated.value = 0)

    L244.StubTechCalInput_CHINAbld <- bind_rows(L244.StubTechCalInput_CHINAbld,
                                                L244.StubTechCalInput_CHINAbld.nocal)

    L244.StubTechCalInput_CHINAbld <- L244.StubTechCalInput_CHINAbld %>%
      mutate(share.weight.year = year,
             calOutputValue = calibrated.value) %>%
      set_subsector_shrwt() %>%
      mutate(tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]])


    # L244.GlobalTechShrwt_CHINAbld: Default shareweights for global building technologies
    L244.GlobalTechShrwt_CHINAbld <- A44.globaltech_shrwt %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], share.weight)

    # L244.GlobalTechInterpTo_CHINAbld: Technology shareweight interpolation (selected techs only)
    # NOTE: There are currently no techs in this table (empty)
    L244.GlobalTechInterpTo_CHINAbld <- A44.globaltech_interp %>%
      set_years() %>%
      mutate(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechInterpTo"]])

    # L244.GlobalTechCost_CHINAbld: Non-fuel costs of global building technologies
    L244.GlobalTechCost_CHINAbld <- A44.globaltech_cost %>%
      gather_years(value_col = "input.cost") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(input.cost = approx_fun(year, input.cost)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      mutate(minicam.non.energy.input = "non-energy") %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]])

    # L244.GlobalTechSCurve_CHINAbld: Retirement rates for building technologies
    L244.GlobalTechSCurve_CHINAbld <- L244.GlobalTechCost_CHINAbld %>%
      filter(year %in% c(max(MODEL_BASE_YEARS), MODEL_FUTURE_YEARS),
             sector.name %in% A44.globaltech_retirement$supplysector) %>%
      # Add lifetimes and steepness
      left_join_error_no_match(A44.globaltech_retirement, by = c("sector.name" = "supplysector")) %>%
      # Set steepness/halflife values to stock for base years, new for future years
      mutate(steepness = if_else(year == max(MODEL_BASE_YEARS), steepness_stock, steepness_new),
             half.life = if_else(year == max(MODEL_BASE_YEARS), half_life_stock, half_life_new)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechSCurve"]])

    # L244.GlobalTechIntGainOutputRatio_CHINAbld: Output ratios of internal gain energy from non-thermal building services
    calibrated_techs_bld_china_consumer <- calibrated_techs_bld_china %>%
      select(gcam.consumer = sector, supplysector) %>%
      distinct()

    L244.GlobalTechIntGainOutputRatio_CHINAbld <- A44.globaltech_intgains %>%
      repeat_add_columns(tibble(year = MODEL_YEARS))%>%
      # Add gcam.consumer (sector)
      left_join_error_no_match(calibrated_techs_bld_china_consumer, by = "supplysector") %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      # Add internal.gains.market.name
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer") %>%
      # Add efficiency
      left_join_error_no_match(L244.GlobalTechEff_CHINAbld,
                               by = c("sector.name", "subsector.name", "technology", "year")) %>%
      mutate(internal.gains.output.ratio = round(input.ratio / efficiency, energy.DIGITS_EFFICIENCY)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], internal.gains.output.ratio, internal.gains.market.name)

    # L244.GenericBaseService and L244.ThermalBaseService: Base year output of buildings services (per unit floorspace)
    # Base-service: Multiply energy consumption by efficiency for each technology, and aggregate by service
    L244.base_service <- L244.StubTechCalInput_CHINAbld %>%
      # Add in efficiency by technology
      left_join_error_no_match(L244.GlobalTechEff_CHINAbld,
                               by = c("supplysector" = "sector.name", "subsector" = "subsector.name",
                                      "stub.technology" = "technology", "year", "minicam.energy.input")) %>%
      # Calculate base.service = calibrated.value(energy) * efficiency
      mutate(base.service = round(calibrated.value * efficiency, energy.DIGITS_CALOUTPUT)) %>%
      # Aggregate base service by service (supplysector)
      group_by(region, supplysector, year) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup() %>%
      # Add gcam.consumer (sector)
      left_join_error_no_match(calibrated_techs_bld_china_consumer, by = "supplysector") %>%
      # Add nodeInput and building.node.input
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer")

    # Separate thermal and generic services into separate tables with different ID strings
    L244.GenericBaseService_CHINAbld <- L244.base_service %>%
      filter(supplysector %in% generic_services) %>%
      rename(building.service.input = supplysector) %>%
      select(LEVEL2_DATA_NAMES[["GenericBaseService"]])

    L244.ThermalBaseService_CHINAbld <- L244.base_service %>%
      filter(supplysector %in% thermal_services) %>%
      rename(thermal.building.service.input = supplysector) %>%
      select(LEVEL2_DATA_NAMES[["ThermalBaseService"]])

    # L244.GenericServiceSatiation_CHINAbld: Satiation levels assumed for non-thermal building services
    # Just multiply the base-service by an exogenous multiplier
    L244.GenericServiceSatiation_CHINAbld <- L244.GenericBaseService_CHINAbld %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      # Add floorspace
      # Must use left join, HK and MC have NA base building size, this is retained
      left_join(L244.Floorspace_CHINAbld, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "year")) %>%
      na.omit() %>%
      # Add multiplier
      left_join_error_no_match(A44.demand_satiation_mult, by = c("building.service.input" = "supplysector")) %>%
      # Satiation level = service per floorspace * multiplier
      mutate(satiation.level = round(base.service / base.building.size * multiplier, energy.DIGITS_COEFFICIENT)) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]])

    # L244.ThermalServiceSatiation_CHINAbld: Satiation levels assumed for thermal building services
    L244.ThermalServiceSatiation_CHINAbld <- L244.ThermalBaseService_CHINAbld %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      # Add floorspace
      # Must use left join, HK and MC have NA base building size, this is retained
      left_join(L244.Floorspace_CHINAbld, by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "year")) %>%
      na.omit() %>%
      # Add multiplier
      left_join_error_no_match(A44.demand_satiation_mult, by = c("thermal.building.service.input" = "supplysector")) %>%
      # Satiation level = service per floorspace * multiplier
      mutate(satiation.level = round(base.service / base.building.size * multiplier, energy.DIGITS_COEFFICIENT)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceSatiation"]])

    # L244.Intgains_scalar: Scalers relating internal gain energy to increased/reduced cooling/heating demands
    variable <- c("HDD", "CDD")
    scalar <- c(energy.INTERNAL_GAINS_SCALAR_USA_H, energy.INTERNAL_GAINS_SCALAR_USA_C)  #NOTE: using USA values here
    DDnorm <- c(gcamusa.BASE_HDD_USA, gcamusa.BASE_CDD_USA) #NOTE: using USA values here - is this appropriate?
    CHINA.base.scalar <- tibble(variable, scalar, DDnorm)

    # First, separate the thermal from the generic services. Generic services will be assumed to produce
    # internal gain energy, so anything in the internal gains assumptions table will be assumed generic
    generic_services <- unique(A44.globaltech_intgains$supplysector)
    thermal_services <- dplyr::setdiff(unique(A44.sector$supplysector), generic_services)

    heating_services <- thermal_services[grepl("heating", thermal_services)]
    cooling_services <- thermal_services[grepl("cooling", thermal_services)]

    L244.Intgains_scalar_CHINAbld <- L244.ThermalServiceSatiation_CHINAbld %>%
      # Assign HDD or CDD
      mutate(variable = if_else(thermal.building.service.input %in% heating_services, "HDD", "CDD")) %>%
      # Add DDnorm & scalar
      left_join_error_no_match(CHINA.base.scalar, by = "variable") %>%
      # TODO: get degree days
      mutate(degree.days = DDnorm,
             internal.gains.scalar = round(scalar * degree.days / DDnorm, energy.DIGITS_HDDCDD)) %>%
      select(LEVEL2_DATA_NAMES[["Intgains_scalar"]])

    # Fuel preference elasticity
    # L244.FuelPrefElast_CHINAbld: fuel preference elasticities of building energy use" )
    L244.FuelPrefElast_CHINAbld <- A44.fuelprefElasticity %>%
      mutate(year.fillout = min(MODEL_FUTURE_YEARS)) %>%
      write_to_all_provinces( LEVEL2_DATA_NAMES[["FuelPrefElast"]], gcamchina.PROVINCES_ALL)



    # ===================================================
    # Produce outputs
    L244.DeleteConsumer_CHINAbld %>%
      add_title("Deletes building sector in China region to rewrite with GCAM-CHINA data") %>%
      add_units("NA") %>%
      add_comments("gcam.consumer column from A44.gcam_consumer") %>%
      add_legacy_name("L244.DeleteConsumer_CHINAbld") %>%
      add_precursors("energy/A44.gcam_consumer") ->
      L244.DeleteConsumer_CHINAbld

    L244.DeleteSupplysector_CHINAbld %>%
      add_title("Deletes building sector in CHINA region to rewrite with GCAM-CHINA data") %>%
      add_units("NA") %>%
      add_comments("supplysector column from A44.sector") %>%
      add_legacy_name("L244.DeleteSupplysector_CHINAbld") %>%
      add_precursors("energy/A44.sector") ->
      L244.DeleteSupplysector_CHINAbld

    L244.SubregionalShares_CHINAbld %>%
      add_title("Subregional population and income shares") %>%
      add_units("Unitless") %>%
      add_comments("Default values used for years and shares") %>%
      add_legacy_name("L244.SubregionalShares") %>%
      add_precursors("gcam-china/A44.gcam_consumer") ->
      L244.SubregionalShares_CHINAbld

    L244.PriceExp_IntGains_CHINAbld %>%
      add_title("Price exponent on floorspace and naming of internal gains trial markets") %>%
      add_units("Unitless") %>%
      add_comments("A44.gcam_consumer written to all provinces") %>%
      add_legacy_name("L244.PriceExp_IntGains") %>%
      add_precursors("gcam-china/A44.gcam_consumer") ->
      L244.PriceExp_IntGains_CHINAbld

    L244.Floorspace_CHINAbld %>%
      add_title("base year floorspace") %>%
      add_units("billion m2") %>%
      add_comments("Data from L144.flsp_bm2_province_bld") %>%
      add_legacy_name("L244.Floorspace") %>%
      add_precursors("L144.flsp_bm2_province_bld", "gcam-china/A44.gcam_consumer") ->
      L244.Floorspace_CHINAbld

    L244.DemandFunction_serv_CHINAbld %>%
      add_title("Service demand function types") %>%
      add_units("NA") %>%
      add_comments("A44.demandFn_serv written to all provinces") %>%
      add_legacy_name("L244.DemandFunction_serv") %>%
      add_precursors("gcam-china/A44.demandFn_serv") ->
      L244.DemandFunction_serv_CHINAbld

    L244.DemandFunction_flsp_CHINAbld %>%
      add_title("Floorspace demand function types") %>%
      add_units("NA") %>%
      add_comments("A44.demandFn_flsp written to all provinces") %>%
      add_legacy_name("L244.DemandFunction_flsp") %>%
      add_precursors("gcam-china/A44.demandFn_flsp") ->
      L244.DemandFunction_flsp_CHINAbld

    L244.Satiation_flsp_CHINAbld %>%
      add_title("Satiation levels assumed for floorspace") %>%
      add_units("million m2 / person") %>%
      add_comments("Values from A44.satiation_flsp or L244.Floorspace_CHINAbld/L101.Pop_thous_province") %>%
      add_comments("Whichever is larger") %>%
      add_legacy_name("L244.Satiation_flsp") %>%
      add_precursors("gcam-china/A44.satiation_flsp", "gcam-china/A44.gcam_consumer", "L101.Pop_thous_province",
                     "L144.flsp_bm2_province_bld") ->
      L244.Satiation_flsp_CHINAbld

    L244.SatiationAdder_CHINAbld %>%
      add_title("Satiation adders in floorspace demand function") %>%
      add_units("million m2 / person") %>%
      add_comments("Calculated with function dependent on satiation level; per capita floorspace; and per capita GDP") %>%
      add_legacy_name("L244.SatiationAdder") %>%
      add_precursors("gcam-china/A44.satiation_flsp", "gcam-china/A44.gcam_consumer", "L101.Pop_thous_province",
                     "L144.flsp_bm2_province_bld", "L101.pcGDP_thous90usd_province") ->
      L244.SatiationAdder_CHINAbld

    L244.ThermalBaseService_CHINAbld %>%
      add_title("Base year output of thermal buildings services") %>%
      add_units("EJ per unit floorspace") %>%
      add_comments("Multiplied energy consumption by efficiency for each technology, then aggregated by service") %>%
      add_legacy_name("L244.ThermalBaseService") %>%
      add_precursors("L144.in_EJ_province_bld_F_U" , "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.globaltech_eff","gcam-china/A44.gcam_consumer") ->
      L244.ThermalBaseService_CHINAbld

    L244.GenericBaseService_CHINAbld %>%
      add_title("Base year output of generic buildings services") %>%
      add_units("EJ per unit floorspace") %>%
      add_comments("Multiplied energy consumption by efficiency for each technology, then aggregated by service") %>%
      add_legacy_name("L244.GenericBaseService") %>%
      add_precursors("L144.in_EJ_province_bld_F_U", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.globaltech_eff","gcam-china/A44.gcam_consumer") ->
      L244.GenericBaseService_CHINAbld

    L244.GenericServiceSatiation_CHINAbld %>%
      add_title("Satiation levels assumed for non-thermal building services") %>%
      add_units("EJ/billion m2 floorspace") %>%
      add_comments("Satiation level = base service / floorspace * exogenous multiplier") %>%
      add_legacy_name("L244.GenericServiceSatiation") %>%
      add_precursors("L144.in_EJ_province_bld_F_U", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.globaltech_eff",
                     "gcam-china/A44.gcam_consumer", "L144.flsp_bm2_province_bld",
                     "gcam-china/A44.demand_satiation_mult") ->
      L244.GenericServiceSatiation_CHINAbld

    L244.ThermalServiceSatiation_CHINAbld %>%
      add_title("Satiation levels assumed for thermal building services") %>%
      add_units("EJ/billion m2 floorspace") %>%
      add_comments("Satiation level = base service / floorspace * exogenous multiplier") %>%
      add_legacy_name("L244.ThermalServiceSatiation") %>%
      add_precursors("L144.in_EJ_province_bld_F_U", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.globaltech_eff",
                     "gcam-china/A44.gcam_consumer", "L144.flsp_bm2_province_bld",
                     "gcam-china/A44.demand_satiation_mult") ->
      L244.ThermalServiceSatiation_CHINAbld

    L244.Intgains_scalar_CHINAbld %>%
      add_title("Scalers relating internal gain energy to increased/reduced cooling/heating demands") %>%
      add_units("Unitless") %>%
      add_comments("internal.gains.scalar = exogenous scalar (as of now)") %>%
      add_legacy_name("L244.Intgains_scalar") %>%
      add_precursors("L144.in_EJ_province_bld_F_U", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.globaltech_eff",
                     "gcam-china/A44.gcam_consumer", "L144.flsp_bm2_province_bld",
                     "gcam-china/A44.demand_satiation_mult") ->
      L244.Intgains_scalar_CHINAbld

    L244.ShellConductance_CHINAbld %>%
      add_title("Shell conductance (inverse of shell efficiency) by province") %>%
      add_units("Unitless") %>%
      add_comments("values from A44.bld_shell_conductance") %>%
      add_legacy_name("L244.ShellConductance_bld") %>%
      add_precursors("gcam-china/A44.bld_shell_conductance", "gcam-china/A44.gcam_consumer") ->
      L244.ShellConductance_CHINAbld

    L244.Supplysector_CHINAbld %>%
      add_title("Supplysector info for buildings") %>%
      add_units("Unitless") %>%
      add_comments("A44.sector written to all provinces") %>%
      add_legacy_name("L244.Supplysector_bld") %>%
      add_precursors("gcam-china/A44.sector") ->
      L244.Supplysector_CHINAbld

    L244.FinalEnergyKeyword_CHINAbld %>%
      add_title("Supply sector keywords for detailed building sector") %>%
      add_units("NA") %>%
      add_comments("A44.sector written to all provinces") %>%
      add_legacy_name("L244.FinalEnergyKeyword_bld") %>%
      add_precursors("gcam-china/A44.sector") ->
      L244.FinalEnergyKeyword_CHINAbld

    if(exists("L244.SubsectorShrwt_CHINAbld")) {
      L244.SubsectorShrwt_CHINAbld %>%
        add_title("Subsector shareweights of building sector") %>%
        add_units("Unitless") %>%
        add_comments("A44.subsector_shrwt written to all regions") %>%
        add_legacy_name("L244.SubsectorShrwt_bld") %>%
        add_precursors("gcam-china/A44.subsector_shrwt") ->
        L244.SubsectorShrwt_CHINAbld
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorShrwt_bld") ->
        L244.SubsectorShrwt_CHINAbld
    }

    if(exists("L244.SubsectorShrwtFllt_CHINAbld")) {
      L244.SubsectorShrwtFllt_CHINAbld %>%
        add_title("Subsector shareweights of building sector") %>%
        add_units("Unitless") %>%
        add_comments("A44.subsector_shrwt written to all regions") %>%
        add_legacy_name("L244.SubsectorShrwtFllt_bld") %>%
        add_precursors("gcam-china/A44.subsector_shrwt") ->
        L244.SubsectorShrwtFllt_CHINAbld
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorShrwtFllt_bld") ->
        L244.SubsectorShrwtFllt_CHINAbld
    }


    if(exists("L244.SubsectorInterp_CHINAbld")) {
      L244.SubsectorInterp_CHINAbld %>%
        add_title("Subsector shareweight interpolation of building sector") %>%
        add_units("NA") %>%
        add_comments("A44.subsector_interp written to all regions") %>%
        add_legacy_name("L244.SubsectorInterp_bld") %>%
        add_precursors("gcam-china/A44.subsector_interp") ->
        L244.SubsectorInterp_CHINAbld
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorInterp_bld") ->
        L244.SubsectorInterp_CHINAbld
    }

    if(exists("L244.SubsectorInterpTo_CHINAbld")) {
      L244.SubsectorInterpTo_CHINAbld %>%
        add_title("Subsector shareweight interpolation of building sector") %>%
        add_units("NA") %>%
        add_comments("A44.subsector_interp written to all regions") %>%
        add_legacy_name("L244.SubsectorInterpTo_bld") %>%
        add_precursors("gcam-china/A44.subsector_interp") ->
        L244.SubsectorInterpTo_CHINAbld
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorInterpTo_bld") ->
        L244.SubsectorInterpTo_CHINAbld
    }

    L244.SubsectorLogit_CHINAbld %>%
      add_title("Subsector logit exponents of building sector") %>%
      add_units("Unitless") %>%
      add_comments("A44.subsector_logit written to all provinces") %>%
      add_legacy_name("L244.SubsectorLogit_bld") %>%
      add_precursors("gcam-china/A44.subsector_logit") ->
      L244.SubsectorLogit_CHINAbld

    L244.StubTech_CHINAbld %>%
      add_title("Identification of stub technologies for buildings") %>%
      add_units("NA") %>%
      add_comments("A44.globaltech_eff written to all provinces") %>%
      add_legacy_name("L244.StubTech_bld") %>%
      add_precursors("gcam-china/A44.globaltech_eff") ->
      L244.StubTech_CHINAbld

    L244.StubTechCalInput_CHINAbld %>%
      add_title("Calibrated energy consumption and share weights by buildings technologies") %>%
      add_units("calibrated.value: EJ/yr; shareweights: Unitless") %>%
      add_comments("Energy consumption multiplied by shares to get calibrated energy") %>%
      add_comments("Shares calculated using efficiency averages") %>%
      add_legacy_name("L244.StubTechCalInput_bld") %>%
      add_precursors("L144.in_EJ_province_bld_F_U", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.globaltech_eff") ->
      L244.StubTechCalInput_CHINAbld

    L244.StubTechMarket_CHINAbld %>%
      add_title("market names for fuel inputs to all technologies in each province") %>%
      add_units("NA") %>%
      add_comments("Categories from A44.globaltech_eff written to all provinces") %>%
      add_comments("Market set to provinces for electricity") %>%
      add_legacy_name("L244.StubTechMarket_bld") %>%
      add_precursors("gcam-china/province_names_mappings") ->
      L244.StubTechMarket_CHINAbld

    L244.GlobalTechIntGainOutputRatio_CHINAbld %>%
      add_title("Output ratios of internal gain energy from non-thermal building services") %>%
      add_units("Unitless") %>%
      add_comments("internal.gains.output.ratio = input.ratio from A44.globaltech_intgains divided by efficiency from L244.GlobalTechEff_bld") %>%
      add_legacy_name("L244.GlobalTechIntGainOutputRatio") %>%
      add_precursors("gcam-china/A44.globaltech_intgains", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.gcam_consumer", "gcam-china/A44.globaltech_eff") ->
      L244.GlobalTechIntGainOutputRatio_CHINAbld

    L244.GlobalTechInterpTo_CHINAbld %>%
      add_title("Technology shareweight interpolation") %>%
      add_units("NA") %>%
      add_comments("Directly from A44.globaltech_interp") %>%
      add_legacy_name("L244.GlobalTechInterpTo_bld") %>%
      add_precursors("gcam-china/A44.globaltech_interp") ->
      L244.GlobalTechInterpTo_CHINAbld

    L244.GlobalTechEff_CHINAbld %>%
      add_title("Assumed efficiencies (all years) of buildings technologies") %>%
      add_units("Unitless") %>%
      add_comments("Values from A44.globaltech_eff") %>%
      add_legacy_name("L244.GlobalTechEff_bld") %>%
      add_precursors("gcam-china/A44.globaltech_eff") ->
      L244.GlobalTechEff_CHINAbld

    L244.GlobalTechShrwt_CHINAbld %>%
      add_title("Default shareweights for global building technologies") %>%
      add_units("Unitless") %>%
      add_comments("Values interpolated to model years from A44.globaltech_shrwt") %>%
      add_legacy_name("L244.GlobalTechShrwt_bld") %>%
      add_precursors("gcam-china/A44.globaltech_shrwt") ->
      L244.GlobalTechShrwt_CHINAbld

    L244.GlobalTechCost_CHINAbld %>%
      add_title("Non-fuel costs of global building technologies") %>%
      add_units("1975$/GJ") %>%
      add_comments("Values from A44.globaltech_cost") %>%
      add_legacy_name("L244.GlobalTechCost_bld") %>%
      add_precursors("gcam-china/A44.globaltech_cost") ->
      L244.GlobalTechCost_CHINAbld

    L244.GlobalTechSCurve_CHINAbld %>%
      add_title("Retirement rates for building technologies") %>%
      add_units("lifetime/half.life = years") %>%
      add_comments("Lifetime, steepness, and half.life from A44.globaltech_retirement") %>%
      add_legacy_name("L244.GlobalTechSCurve_bld") %>%
      add_precursors("gcam-china/A44.globaltech_cost", "gcam-china/A44.globaltech_retirement") ->
      L244.GlobalTechSCurve_CHINAbld

    L244.FuelPrefElast_CHINAbld %>%
      add_title("fuel preference elasticities of building energy use") %>%
      add_units("Unitless") %>%
      add_comments("from A44.fuelprefElasticity") %>%
      add_legacy_name("L244.FuelPrefElast_bld") %>%
      add_precursors("gcam-china/A44.fuelprefElasticity") ->
      L244.FuelPrefElast_CHINAbld

    return_data(L244.DeleteConsumer_CHINAbld,
                L244.DeleteSupplysector_CHINAbld,
                L244.SubregionalShares_CHINAbld,
                L244.PriceExp_IntGains_CHINAbld,
                L244.Floorspace_CHINAbld,
                L244.DemandFunction_serv_CHINAbld,
                L244.DemandFunction_flsp_CHINAbld,
                L244.Satiation_flsp_CHINAbld,
                L244.SatiationAdder_CHINAbld,
                L244.ThermalBaseService_CHINAbld,
                L244.GenericBaseService_CHINAbld,
                L244.ThermalServiceSatiation_CHINAbld,
                L244.GenericServiceSatiation_CHINAbld,
                L244.Intgains_scalar_CHINAbld,
                L244.ShellConductance_CHINAbld,
                L244.Supplysector_CHINAbld,
                L244.FinalEnergyKeyword_CHINAbld,
                L244.SubsectorShrwt_CHINAbld,
                L244.SubsectorShrwtFllt_CHINAbld,
                L244.SubsectorInterp_CHINAbld,
                L244.SubsectorInterpTo_CHINAbld,
                L244.SubsectorLogit_CHINAbld,
                L244.StubTech_CHINAbld,
                L244.StubTechCalInput_CHINAbld,
                L244.StubTechMarket_CHINAbld,
                L244.GlobalTechIntGainOutputRatio_CHINAbld,
                L244.GlobalTechInterpTo_CHINAbld,
                L244.GlobalTechEff_CHINAbld,
                L244.GlobalTechShrwt_CHINAbld,
                L244.GlobalTechCost_CHINAbld,
                L244.GlobalTechSCurve_CHINAbld,
                L244.FuelPrefElast_CHINAbld)
  } else {
    stop("Unknown command")
  }
}
