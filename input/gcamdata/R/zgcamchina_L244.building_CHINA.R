# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L244.building
#'
#' Creates GCAM-CHINA building output files for writing to xml.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L244.DeleteConsumer_CHINAbld}, \code{L244.DeleteSupplysector_CHINAbld}, \code{L244.SubregionalShares_gcamchina},
#' \code{L244.PriceExp_IntGains_gcamchina}, \code{L244.Floorspace_gcamchina}, \code{L244.DemandFunction_serv_gcamchina}, \code{L244.DemandFunction_flsp_gcamchina},
#' \code{L244.Satiation_flsp_gcamchina}, \code{L244.SatiationAdder_gcamchina}, \code{L244.ThermalBaseService_gcamchina}, \code{L244.GenericBaseService_gcamchina},
#' \code{L244.ThermalServiceSatiation_gcamchina}, \code{L244.GenericServiceSatiation_gcamchina}, \code{L244.Intgains_scalar_gcamchina},
#' \code{L244.ShellConductance_bld_gcamchina}, \code{L244.Supplysector_bld_gcamchina}, \code{L244.FinalEnergyKeyword_bld_gcamchina}, \code{L244.SubsectorShrwt_bld_gcamchina},
#' \code{L244.SubsectorShrwtFllt_bld_gcamchina}, \code{L244.SubsectorInterp_bld_gcamchina}, \code{L244.SubsectorInterpTo_bld_gcamchina},
#' \code{L244.SubsectorLogit_bld_gcamchina}, \code{L244.StubTech_bld_gcamchina}, \code{L244.StubTechCalInput_bld_gcamchina}, \code{L244.StubTechMarket_bld},
#' \code{L244.GlobalTechIntGainOutputRatio_gcamchina}, \code{L244.GlobalTechInterpTo_bld_gcamchina}, \code{L244.GlobalTechEff_bld_gcamchina},
#' \code{L244.GlobalTechShrwt_bld_gcamchina}, \code{L244.GlobalTechCost_bld_gcamchina}, \code{L244.GlobalTechSCurve_bld_gcamchina}, \code{L244.HDDCDD_A2_CCSM3x_China},
#' \code{L244.HDDCDD_constdds_China}, \code{L244.GompFnParam_gcamchina}, \code{L244.Satiation_impedance_gcamchina},
#' \code{L244.GenericServiceImpedance_gcamchina},\code{L244.GenericServiceCoef_gcamchina},\code{L244.GenericServiceAdder_gcamchina},
#' \code{L244.ThermalServiceImpedance_gcamchina},\code{L244.ThermalServiceCoef_gcamchina},\code{L244.ThermalServiceAdder_gcamchina},
#' \code{L244.GenericServicePrice_gcamchina}, \code{L244.ThermalServicePrice_gcamchina},
#' \code{L244.GenericBaseDens_gcamchina}, \code{L244.ThermalBaseDens_gcamchina},
#' \code{L244.DeleteThermalService_gcamchina},\code{L244.DeleteGenericService_gcamchina},\code{L244.ThermalCoalCoef_gcamchina},
#' \code{L244.ThermalTradBioCoef_gcamchina},\code{L244.GenericShares_gcamchina},\code{L244.ThermalShares_gcamchina},
#' \code{L244.FuelPrefElast_bld_gcamchina},
#' The corresponding file in the original data system was \code{L244.building_CHina.R} (gcam-china level2).
#' @details Creates GCAM-CHINA building output files for writing to xml.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join mutate select semi_join summarise
#' @importFrom tidyr complete gather nesting replace_na
#' @author BY January 2020, Rongqi April 2024 , YangLiu Oct 2024

module_gcamchina_L244.building <- function(command, ...) {
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
             FILE = "gcam-china/A44.globaltech_eff_avg",
             FILE = "gcam-china/A44.globaltech_shares",
             FILE = "gcam-china/A44.globaltech_intgains",
             FILE = "gcam-china/A44.globaltech_retirement",
             FILE = "gcam-china/A44.globaltech_shrwt",
             FILE = "gcam-china/A44.globaltech_interp",
             FILE = "gcam-china/A44.demand_satiation_mult",
             FILE = "gcam-china/A44.hab_land_flsp_china",
             FILE = "socioeconomics/income_shares",
             FILE = "gcam-china/A44.CalPrice_service_gcamchina",
             FILE = "gcam-china/A44.fuelprefElasticity",
             FILE = "gcam-china/province_decile_gdp_per_capita_projections",
             FILE = "gcam-china/urban_pop_share_province",
             FILE = "gcam-china/urban_income_share_province",
             "L244.Supplysector_bld",
             "L144.flsp_param",
             "L144.flsp_bm2_province_bld",
             "L144.in_EJ_province_bld_F_U",
             "L143.HDDCDD_scen_R_Y",
             "L101.Pop_thous_province",
             "L101.pcGDP_thous90usd_province"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L244.DeleteConsumer_CHINAbld",
             "L244.DeleteSupplysector_CHINAbld",
             "L244.SubregionalShares_gcamchina",
             "L244.PriceExp_IntGains_gcamchina",
             "L244.Floorspace_gcamchina",
             "L244.DemandFunction_serv_gcamchina",
             "L244.DemandFunction_flsp_gcamchina",
             "L244.Satiation_flsp_gcamchina",
             "L244.SatiationAdder_gcamchina",
             "L244.StubTech_bld_gcamchina",
             "L244.StubTechCalInput_bld_gcamchina",
             "L244.ThermalBaseService_gcamchina",
             "L244.GenericBaseService_gcamchina",
             "L244.ThermalServiceSatiation_gcamchina",
             "L244.GenericServiceSatiation_gcamchina",
             "L244.Intgains_scalar_gcamchina",
             "L244.ShellConductance_bld_gcamchina",
             "L244.Supplysector_bld_gcamchina",
             "L244.FinalEnergyKeyword_bld_gcamchina",
             "L244.SubsectorShrwt_bld_gcamchina",
             "L244.SubsectorShrwtFllt_bld_gcamchina",
             "L244.SubsectorInterp_bld_gcamchina",
             "L244.SubsectorInterpTo_bld_gcamchina",
             "L244.SubsectorLogit_bld_gcamchina",
             "L244.StubTechMarket_bld_gcamchina",
             "L244.GlobalTechIntGainOutputRatio_gcamchina",
             "L244.GlobalTechInterpTo_bld_gcamchina",
             "L244.GlobalTechEff_bld_gcamchina",
             "L244.GlobalTechShrwt_bld_gcamchina",
             "L244.GlobalTechCost_bld_gcamchina",
             "L244.GlobalTechSCurve_bld_gcamchina",
             "L244.HDDCDD_A2_CCSM3x_China",
             "L244.HDDCDD_constdds_China",
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
             "L244.GenericShares_gcamchina",
             "L244.ThermalShares_gcamchina",
             "L244.FuelPrefElast_bld_gcamchina"
    ))

  } else if(command == driver.MAKE) {

    # Silence package checks
    GCM <- Scen <- base.building.size <- base.service <- calibrated.value <- comm <-
      degree.days <- efficiency <- efficiency_tech1 <- efficiency_tech2 <- fuel <-
      gcam.consumer <- grid_region <- half_life_new <- half_life_stock <- input.cost <-
      input.ratio <- internal.gains.market.name <- internal.gains.output.ratio <-
      internal.gains.scalar <- market.name <- minicam.energy.input <- multiplier <-
      object <- pcFlsp_mm2 <- pcGDP <- pcflsp_mm2cap <- pop <- region <- resid <-
      satiation.adder <- satiation.level <- sector <- sector.name <- service <- share <-
      share.weight <- share_tech1 <- share_tech2 <- share_type <- state <- steepness_new <-
      steepness_stock <- stockavg <- subsector <- subsector.name <- supplysector <-
      tech_type <- technology <- technology1 <- technology2 <-
      thermal.building.service.input <- to.value <- value <- year <- year.fillout <- . <-
      pop_year <- Sector <- pop_share <- growth <- flsp_growth <- area_gcam <- misc_land_gtdc <-
      area_thouskm2 <- flsp <- pop_thous <- flsp_pc <- tot.dens <- unadjust.satiation <-
      land.density.param <- b.param <- income.param <- gdp_pc <- flsp_est <- base_flsp <-
      bias.adjust.param <- province_name <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    A44.gcam_consumer_en <- get_data(all_data, "energy/A44.gcam_consumer", strip_attributes = TRUE)
    A44.sector_en <- get_data(all_data, "energy/A44.sector", strip_attributes = TRUE)
    calibrated_techs_bld_china <- get_data(all_data, "gcam-china/calibrated_techs_bld_china", strip_attributes = TRUE)
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = TRUE)
    A44.bld_shell_conductance <- get_data(all_data, "gcam-china/A44.bld_shell_conductance", strip_attributes = TRUE)
    A44.demandFn_flsp <- get_data(all_data, "gcam-china/A44.demandFn_flsp", strip_attributes = TRUE)
    A44.demandFn_serv <- get_data(all_data, "gcam-china/A44.demandFn_serv", strip_attributes = TRUE)
    A44.gcam_consumer <- get_data(all_data, "gcam-china/A44.gcam_consumer", strip_attributes = TRUE)
    A44.satiation_flsp <- get_data(all_data, "gcam-china/A44.satiation_flsp", strip_attributes = TRUE)
    A44.sector <- get_data(all_data, "gcam-china/A44.sector", strip_attributes = TRUE)
    A44.subsector_interp <- get_data(all_data, "gcam-china/A44.subsector_interp", strip_attributes = TRUE)
    A44.subsector_logit <- get_data(all_data, "gcam-china/A44.subsector_logit", strip_attributes = TRUE)
    A44.subsector_shrwt <- get_data(all_data, "gcam-china/A44.subsector_shrwt", strip_attributes = TRUE)
    A44.globaltech_cost <- get_data(all_data, "gcam-china/A44.globaltech_cost", strip_attributes = TRUE)
    A44.globaltech_eff <- get_data(all_data, "gcam-china/A44.globaltech_eff", strip_attributes = TRUE) %>%
      gather_years()
    A44.globaltech_eff_avg <- get_data(all_data, "gcam-china/A44.globaltech_eff_avg", strip_attributes = TRUE)
    A44.globaltech_shares <- get_data(all_data, "gcam-china/A44.globaltech_shares", strip_attributes = TRUE)
    A44.globaltech_intgains <- get_data(all_data, "gcam-china/A44.globaltech_intgains", strip_attributes = TRUE)
    A44.globaltech_retirement <- get_data(all_data, "gcam-china/A44.globaltech_retirement", strip_attributes = TRUE)
    A44.globaltech_shrwt <- get_data(all_data, "gcam-china/A44.globaltech_shrwt", strip_attributes = TRUE)
    A44.globaltech_interp <- get_data(all_data, "gcam-china/A44.globaltech_interp", strip_attributes = TRUE)
    A44.demand_satiation_mult <- get_data(all_data, "gcam-china/A44.demand_satiation_mult", strip_attributes = TRUE)
    A44.fuelprefElasticity <- get_data(all_data, "gcam-china/A44.fuelprefElasticity", strip_attributes = TRUE)
    L144.hab_land_flsp_china <- get_data(all_data, "gcam-china/A44.hab_land_flsp_china", strip_attributes = TRUE)
    income_shares <- get_data(all_data, "socioeconomics/income_shares")
    L144.prices_bld_gcamchina <- get_data(all_data, "gcam-china/A44.CalPrice_service_gcamchina", strip_attributes = TRUE) %>%
      gather_years()
    province_decile_gdp_per_capita_projections <- get_data(all_data,"gcam-china/province_decile_gdp_per_capita_projections", strip_attributes = TRUE)
    L244.Supplysector_bld <- get_data(all_data, "L244.Supplysector_bld") %>% filter(region == gcamchina.REGION)
    L144.flsp_param <- get_data(all_data, "L144.flsp_param", strip_attributes = TRUE)
    L144.flsp_bm2_province_bld <- get_data(all_data, "L144.flsp_bm2_province_bld", strip_attributes = TRUE)
    L144.in_EJ_province_bld_F_U <- get_data(all_data, "L144.in_EJ_province_bld_F_U", strip_attributes = TRUE)
    L143.HDDCDD_scen_R_Y <- get_data(all_data, "L143.HDDCDD_scen_R_Y", strip_attributes = TRUE)
    L101.Pop_thous_province <- get_data(all_data, "L101.Pop_thous_province", strip_attributes = TRUE)
    L101.pcGDP_thous90usd_province <- get_data(all_data, "L101.pcGDP_thous90usd_province", strip_attributes = TRUE)
    n_groups <- nrow(unique(get_data(all_data, "socioeconomics/income_shares") %>%
                            select(category)))
    urban_pop_share_province <- get_data(all_data, "gcam-china/urban_pop_share_province", strip_attributes = TRUE)
    urban_income_share_province <- get_data(all_data, "gcam-china/urban_income_share_province", strip_attributes = TRUE)


    # Add a deflator for harmonizing GDPpc with prices
    def9075<-2.212

    # ===================================================
    # Rongqi 02/02/2024 update to have different share at province level given new income distribution projections
    # For gcam-consumers: the core version  uses multiple consumers, so this needs to be considered:
    L244.DeleteConsumer_CHINAbld <- tibble(region = gcamchina.REGION, gcam.consumer = A44.gcam_consumer_en$gcam.consumer) %>%
      filter(grepl('resid', gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(income_shares$category))) %>%
      unite(gcam.consumer, c(gcam.consumer,group),sep="_") %>%
      bind_rows(tibble(region = gcamchina.REGION, gcam.consumer = A44.gcam_consumer_en$gcam.consumer)
                %>% filter(gcam.consumer == "comm"))

    # Multiple consumers need to be also added to supplysectors to delete:
    # not distinguish rural and urban
    L244.DeleteSupplysector_CHINAbld <- tibble(region = gcamchina.REGION, supplysector = unique(L244.Supplysector_bld$supplysector))

    # Rongqi 01/02/2024 make similar adjustment as in zchunk_L244.building_det to implement multiple consumer in resid in GCAM-CHINA
    L144.income_shares <- income_shares %>%
      filter(model %in% c(socioeconomics.BASE_INCSHARE_BASE,socioeconomics.BASE_INCSHARE_MODEL)) %>%
      select(-gini,-gdp_pcap_decile,-model) %>%
      rename(group = category,
             scen = sce,
             share = shares)

    # Rongqi 01/02/2024 new income dist projections at province and consumer level (not real data)
    L244.income_shares_province_consumer <- province_decile_gdp_per_capita_projections %>%
      select(province=province_code,year,scen=sce,share=shares,group=category,gcam.consumer=gcam.consumer)


    A44.gcam_consumer<-A44.gcam_consumer %>%
      filter(grepl('resid', gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer, c(gcam.consumer,group),sep="_") %>%
      bind_rows(A44.gcam_consumer %>% filter(gcam.consumer == "comm"))

    # L244.SubregionalShares_gcamchina: subregional population and income shares (not currently used)
    # Rongqi 01/02/2024 modified to use subregional pop and income shares
    L244.gcamchina_consumer_grp_resid <- write_to_all_provinces(A44.gcam_consumer, c("region", "gcam.consumer"), gcamchina.PROVINCES_NOHKMC) %>%
      # filter residential sector to implement multiple consumers
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("gcam.consumer","group"),sep = "_(?=[^_]*$)")

    L244.SubregionalShares_gcamchina0 <- L244.gcamchina_consumer_grp_resid %>%
      repeat_add_columns(tibble(pop.year.fillout=MODEL_YEARS)) %>%
      mutate(inc.year.fillout = pop.year.fillout) %>%
      mutate(GCAM_region_ID = gcamchina.REGION_ID) %>%
      left_join_error_no_match(L144.income_shares %>%
                                 filter(scen %in% c(socioeconomics.BASE_INCSHARE_BASE,socioeconomics.BASE_INCSHARE_SCENARIO)) %>%
                                 rename(pop.year.fillout=year),
                               by=c("GCAM_region_ID","group","pop.year.fillout"))


    # starting 2015 ... 2100 (three SSPs have same income shares in 2015)
    L244.SubregionalShares_gcamchina00 <- L244.SubregionalShares_gcamchina0 %>%
      filter(pop.year.fillout >= MODEL_FINAL_BASE_YEAR) %>%
      select(-share) %>%
      left_join_error_no_match(L244.income_shares_province_consumer %>%
                                 filter(scen %in% c('Historical data',socioeconomics.BASE_INCSHARE_SCENARIO)) %>%
                                 select(-scen),
                               by = c('region'='province','gcam.consumer','group','pop.year.fillout'='year')) %>%
      bind_rows(L244.SubregionalShares_gcamchina0 %>%
                  filter(pop.year.fillout < MODEL_FINAL_BASE_YEAR)) %>%
      mutate(subregional.population.share = 1/n_groups) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      rename(subregional.income.share = share) %>%
      select(-scen,-GCAM_region_ID)

    # Rongqi 23/07/2024
    # Add the difference between urban and rural by subreginal population and income
    urban_pop_share_province.long <- urban_pop_share_province %>%
      gather(key = "year", value = "value", as.character(c(1971:2017, seq(2020, 2100, 5)))) %>%
      mutate(year = as.numeric(year)) %>%
      left_join_error_no_match(province_names_mappings, by = c("province.name")) %>%
      select(province, year, value) %>%
      rename(urban_pop_share = value)

    urban_income_share_province.long <- urban_income_share_province %>%
      gather(key = "year", value = "value", as.character(c(1971:2022, seq(2025, 2100, 5)))) %>%
      mutate(year = as.numeric(year)) %>%
      left_join_error_no_match(province_names_mappings, by = c("province.name")) %>%
      select(province, year, value) %>%
      rename(urban_income_share = value)

    L244.SubregionalShares_gcamchina <- L244.SubregionalShares_gcamchina00 %>%
      left_join_error_no_match(urban_pop_share_province.long, by = c("region"="province", "pop.year.fillout"="year")) %>%
      mutate(subregional.population.share = case_when(
        grepl("resid_urban", gcam.consumer) ~ subregional.population.share * urban_pop_share,
        grepl("resid_rural", gcam.consumer) ~ subregional.population.share * (1-urban_pop_share))) %>%
      select(-urban_pop_share) %>%
      left_join_error_no_match(urban_income_share_province.long, by = c("region"="province", "inc.year.fillout"="year")) %>%
      mutate(subregional.income.share = case_when(
        grepl("resid_urban", gcam.consumer) ~ subregional.income.share * urban_income_share,
        grepl("resid_rural", gcam.consumer) ~ subregional.income.share * (1-urban_income_share))) %>%
      select(-urban_income_share) %>%
      ## 05/21/2024 Rongqi
      # bind commercial subregional population and income shares (currently not used, set to 1)
      bind_rows(write_to_all_provinces(A44.gcam_consumer, c("region", "gcam.consumer"), gcamchina.PROVINCES_NOHKMC) %>%
                  filter(gcam.consumer == "comm") %>%
                  repeat_add_columns(tibble(pop.year.fillout=MODEL_YEARS)) %>%
                  mutate(inc.year.fillout = pop.year.fillout) %>%
                  mutate(subregional.population.share = 1,
                         subregional.income.share = 1)) %>%
      arrange(inc.year.fillout)

    # Create a similar dataframe with all historical years
    # Used to create the historical subregional GDPpc dataframe
    L244.SubregionalShares_gcamchina_allhist0 <- L244.gcamchina_consumer_grp_resid %>%
      repeat_add_columns(tibble(pop.year.fillout=(HISTORICAL_YEARS))) %>%
      mutate(inc.year.fillout=pop.year.fillout) %>%
      mutate(GCAM_region_ID = gcamchina.REGION_ID) %>%
      left_join_error_no_match(L144.income_shares %>%
                                 filter(scen %in% c(socioeconomics.BASE_INCSHARE_BASE,socioeconomics.BASE_INCSHARE_SCENARIO)) %>%
                                 rename(pop.year.fillout=year),
                               by=c("GCAM_region_ID","group","pop.year.fillout"))

    L244.SubregionalShares_gcamchina_allhist00 <- L244.SubregionalShares_gcamchina_allhist0 %>%
      filter(pop.year.fillout >= MODEL_FINAL_BASE_YEAR) %>%
      select(-share) %>%
      left_join_error_no_match(L244.income_shares_province_consumer %>%
                                 filter(scen %in% c('Historical data',socioeconomics.BASE_INCSHARE_SCENARIO)) %>%
                                 select(-scen),
                               by = c('region'='province','gcam.consumer','group','pop.year.fillout'='year')) %>%
      bind_rows(L244.SubregionalShares_gcamchina_allhist0 %>%
                  filter(pop.year.fillout < MODEL_FINAL_BASE_YEAR)) %>%
      mutate(subregional.population.share=1/n_groups) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      rename(subregional.income.share=share) %>%
      select(-scen,-GCAM_region_ID)


    L244.SubregionalShares_gcamchina_allhist <- L244.SubregionalShares_gcamchina_allhist00 %>%
      left_join_error_no_match(urban_pop_share_province.long, by = c("region"="province", "pop.year.fillout"="year")) %>%
      mutate(subregional.population.share = case_when(
        grepl("resid_urban", gcam.consumer) ~ subregional.population.share * urban_pop_share,
        grepl("resid_rural", gcam.consumer) ~ subregional.population.share * (1-urban_pop_share))) %>%
      select(-urban_pop_share) %>%
      left_join_error_no_match(urban_income_share_province.long, by = c("region"="province", "inc.year.fillout"="year")) %>%
      mutate(subregional.income.share = case_when(
        grepl("resid_urban", gcam.consumer) ~ subregional.income.share * urban_income_share,
        grepl("resid_rural", gcam.consumer) ~ subregional.income.share * (1-urban_income_share))) %>%
      select(-urban_income_share) %>%
      ## 05/21/2024 Rongqi
      # bind commercial subregional population and income shares (currently not used, set to 1)
      bind_rows(write_to_all_provinces(A44.gcam_consumer, c("region", "gcam.consumer"), gcamchina.PROVINCES_NOHKMC) %>%
                  filter(gcam.consumer == "comm") %>%
                  repeat_add_columns(tibble(pop.year.fillout=HISTORICAL_YEARS)) %>%
                  mutate(inc.year.fillout = pop.year.fillout) %>%
                  mutate(subregional.population.share = 1,
                         subregional.income.share = 1)) %>%
      arrange(inc.year.fillout)


    # # Generate subregional shares for different SSP scenarios
    # L244.SubregionalShares_gcamchina_SSP0 <- L244.gcamchina_consumer_grp_resid %>%
    #   repeat_add_columns(tibble(pop.year.fillout=MODEL_YEARS)) %>%
    #   mutate(inc.year.fillout = pop.year.fillout) %>%
    #   mutate(GCAM_region_ID = gcamchina.REGION_ID) %>%
    #   repeat_add_columns(tibble(scen = unique(L144.income_shares$scen))) %>%
    #   filter(grepl("SSP",scen)) %>%
    #   filter(pop.year.fillout > MODEL_FINAL_BASE_YEAR) %>%
    #   left_join_error_no_match(L144.income_shares %>%
    #                              rename(pop.year.fillout = year)
    #                            ,by=c("GCAM_region_ID","group","pop.year.fillout","scen"))
    #
    # L244.SubregionalShares_gcamchina_SSP <- L244.SubregionalShares_gcamchina_SSP0 %>%
    #   filter(scen %in% unique(L244.income_shares_province_consumer$scen)) %>%
    #   select(-share) %>%
    #   left_join_error_no_match(L244.income_shares_province_consumer,
    #                            by = c('region'='province','gcam.consumer','group','pop.year.fillout'='year','scen')) %>%
    #   bind_rows(L244.SubregionalShares_gcamchina_SSP0 %>%
    #               filter(!(scen %in% unique(L244.income_shares_province_consumer$scen)))) %>%
    #   mutate(subregional.population.share = 1/n_groups) %>%
    #   unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
    #   rename(subregional.income.share = share,
    #          SSP = scen) %>%
    #   select(-GCAM_region_ID) %>%
    #   split(.$SSP) %>%
    #   lapply(function(df) {
    #     select(df, -SSP) %>%
    #       add_units("Share") %>%
    #       add_comments("Income share for each income group within each region for all SSP scenarios") %>%
    #       add_precursors("socioeconomics/income_shares")
    #   })
    #
    # # Assign each tibble in list
    # for(i in names(L244.SubregionalShares_gcamchina_SSP)) {
    #   assign(paste0("L244.SubregionalShares_gcamchina_", i), L244.SubregionalShares_gcamchina_SSP[[i]] %>%
    #            add_title(paste0("GDP shares: ", i)) %>%
    #            add_legacy_name(paste0("L244.SubregionalShares_gcamchina_", i)))
    # }


    # TODO create chains of variable dependence under SSPs and automatically generate xml under different SSPs
    # L244.SubregionalShares_gcamusa <- L244.SubregionalShares_gcamusa %>%
    #  anti_join(L244.SubregionalShares_gcamusa_SSP3,
    #            by = c('region','gcam.consumer','pop.year.fillout','inc.year.fillout')) %>%
    #  bind_rows(L244.SubregionalShares_gcamusa_SSP3)

    # ===================================================
    # # Rongqi 04/02/2024 uese subregional population and income shares, create datasets with subregional population and per capita income for all GCAM regions:
    # Subregional population per region, year and consumer group
    urban_pop_thous_prov_Yh <- L101.Pop_thous_province %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(urban_pop_share_province.long, by = c("province", "year")) %>%
      mutate(pop = pop * urban_pop_share) %>%
      select(-urban_pop_share)

    rural_pop_thous_prov_Yh <- L101.Pop_thous_province %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(urban_pop_thous_prov_Yh, by = c("province", "year"), suffix = c(".total", ".urban")) %>%
      mutate(pop = pop.total - pop.urban) %>%
      select(-pop.total, -pop.urban)

    L101.Pop_thous_province_Yh_gr <- urban_pop_thous_prov_Yh %>%
      rename(pop_thous = pop) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      repeat_add_columns(tibble(gcam.consumer=paste0("resid_urban_",unique(L144.income_shares$group)))) %>%
      mutate(pop_thous = pop_thous*(1/n_groups)) %>%
      bind_rows(rural_pop_thous_prov_Yh %>%
                  rename(pop_thous = pop) %>%
                  filter(year %in% HISTORICAL_YEARS) %>%
                  repeat_add_columns(tibble(gcam.consumer=paste0("resid_rural_",unique(L144.income_shares$group)))) %>%
                  mutate(pop_thous = pop_thous*(1/n_groups))) %>%
      bind_rows(L101.Pop_thous_province %>%
                  mutate(gcam.consumer = "comm") %>%
                  rename(pop_thous = pop) %>%
                  filter(year %in% HISTORICAL_YEARS))

    # Subregional per capita income per region, year and consumer group
    L101.pcgdp_thous90USD_province_Yh_gr <- L101.pcGDP_thous90usd_province %>%
      rename(pcGDP_thous90USD = pcGDP) %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      left_join_error_no_match(urban_pop_thous_prov_Yh %>%
                                 filter(year %in% HISTORICAL_YEARS),
                               by=c("province","year")) %>%
      rename(pop_thous = pop) %>%
      # 26/07/2024 Rongqi
      # Due to include urban-rural income gap in the L244.SubregionalShares_gcamchina_allhist, use national=level pop to calculate gdp
      left_join_error_no_match(L101.Pop_thous_province %>%
                                 filter(year %in% HISTORICAL_YEARS),
                               by=c("province","year")) %>%
      mutate(gdp = pcGDP_thous90USD * 1E3 * pop * 1E3) %>%
      repeat_add_columns(tibble(gcam.consumer=paste0("resid_urban_",unique(L144.income_shares$group)))) %>%
      mutate(pop_thous = pop_thous * (1/n_groups)) %>%
      bind_rows(L101.pcGDP_thous90usd_province %>%
                  rename(pcGDP_thous90USD = pcGDP) %>%
                  filter(year %in% HISTORICAL_YEARS) %>%
                  left_join_error_no_match(rural_pop_thous_prov_Yh %>%
                                             filter(year %in% HISTORICAL_YEARS),
                                           by=c("province","year")) %>%
                  rename(pop_thous = pop) %>%
                  # 26/07/2024 Rongqi
                  # Due to include urban-rural income gap in the L244.SubregionalShares_gcamchina_allhist, use national=level pop to calculate gdp
                  left_join_error_no_match(L101.Pop_thous_province %>%
                                             filter(year %in% HISTORICAL_YEARS),
                                           by=c("province","year")) %>%
                  mutate(gdp = pcGDP_thous90USD * 1E3 * pop * 1E3) %>%
                  repeat_add_columns(tibble(gcam.consumer=paste0("resid_rural_",unique(L144.income_shares$group)))) %>%
                  mutate(pop_thous = pop_thous * (1/n_groups))) %>%
      left_join_error_no_match(L244.SubregionalShares_gcamchina_allhist %>%
                                 select(-subregional.population.share,-pop.year.fillout) %>%
                                 rename(year = inc.year.fillout)
                               , by=c("province"="region","gcam.consumer","year")) %>%
      mutate(gdp_gr = gdp * subregional.income.share,
             gdp_pc=(gdp_gr/1E3) / (pop_thous*1E3)) %>%
      select(-pcGDP_thous90USD) %>%
      rename(pcGDP_thous90USD = gdp_pc) %>%
      # Rongqi 02/02/2024 pcgdp in provinces does not have scenario (SSPs), not like the L102.pcgdp_thous90USD_Scen_R_Y
      select(province,gcam.consumer,year,pcGDP_thous90USD) %>%
      # add commercial
      bind_rows(L101.pcGDP_thous90usd_province %>%
                  mutate(gcam.consumer = "comm") %>%
                  rename(pcGDP_thous90USD = pcGDP) %>%
                  filter(year %in% HISTORICAL_YEARS))


    # ===================================================
    # Demand function: Need some dataframes to specify floorspace and building energy demand for different gcam.consumers (resid/comm)
    # Demand for floorspace:L244.DemandFunction_flsp
    L244.DemandFunction_flsp_gcamchina <- write_to_all_provinces(A44.demandFn_flsp, LEVEL2_DATA_NAMES[["DemandFunction_flsp"]], gcamchina.PROVINCES_NOHKMC) %>%
      filter(grepl('resid', gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer, c(gcam.consumer,group),sep="_") %>%
      bind_rows(write_to_all_states(A44.demandFn_flsp, LEVEL2_DATA_NAMES[["DemandFunction_flsp"]], gcamchina.PROVINCES_NOHKMC) %>%
                  filter(gcam.consumer == "comm"))

    # Demand for building energy:L244.DemandFunction_serv
    L244.DemandFunction_serv_gcamchina <- write_to_all_provinces(A44.demandFn_serv, LEVEL2_DATA_NAMES[["DemandFunction_serv"]], gcamchina.PROVINCES_NOHKMC) %>%
      filter(grepl('resid', gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer, c(gcam.consumer,group),sep="_") %>%
      bind_rows(write_to_all_provinces(A44.demandFn_serv, LEVEL2_DATA_NAMES[["DemandFunction_serv"]], gcamchina.PROVINCES_NOHKMC)
                %>% filter(gcam.consumer == "comm"))

    # Rongqi 03/02/2024 end ----


    # L244.PriceExp_IntGains_gcamchina: price exponent on floorspace and naming of internal gains trial markets
    L244.PriceExp_IntGains_gcamchina <- write_to_all_provinces(A44.gcam_consumer, LEVEL2_DATA_NAMES[["PriceExp_IntGains"]], gcamchina.PROVINCES_NOHKMC)


    # L244.Floorspace_gcamchina: base year floorspace
    # Keep all historical years for now - these are needed in calculating satiation adders later on
    # ==================================================
    # FLOORSPACE
    # ==================================================
    # 1.Residential floorspace ----
    # L244.Floorspace_resid <- L144.flsp_bm2_state_res %>%
    #   rename(base.building.size = value,
    #          region = state,
    #          gcam.consumer = sector) %>%
    #   repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
    #   unite(gcam.consumer, c(gcam.consumer,group),sep="_") %>%
    #   mutate(base.building.size = round(base.building.size, energy.DIGITS_FLOORSPACE)) %>%
    #   left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer") %>%
    #   select(LEVEL2_DATA_NAMES[["Floorspace"]])

    # Rongqi add 03/02/2024 ----
    A44.gcam_consumer_resid <- A44.gcam_consumer %>%
      filter(grepl("resid", gcam.consumer))

    # Using the parameters estimated in module LA144.building_det_flsp, calculate the "estimated" residential floorspace
    # These estimations will be used for calibration and for the calculation of the regional bias adder (bias-adjust-parameter) in those regions with observed historical data
    L144.flsp_param_china <- L144.flsp_param %>%
      filter(region=="China") %>%
      select(-region) %>%
      repeat_add_columns(tibble(region=gcamchina.PROVINCES_NOHKMC))

    # First calculate the habitable land
    L144.hab_land_flsp_china_fin <- L144.hab_land_flsp_china %>%
      rename(province.name=province)%>%
      left_join(province_names_mappings %>% select(province,province.name),by="province.name") %>%
      filter(province %in% gcamchina.PROVINCES_NOHKMC) %>%
      mutate(area_thouskm2=(area_gcam-misc_land_gtdc)/1E3) %>%
      select(region=province,area_thouskm2)


    L244.Floorspace_resid_est <- L144.flsp_param_china %>%
      repeat_add_columns(tibble(gcam.consumer=paste0("resid_urban_",unique(L144.income_shares$group)))) %>%
      repeat_add_columns(tibble(year=HISTORICAL_YEARS[HISTORICAL_YEARS > 1974])) %>%
      bind_rows(L144.flsp_param_china %>%
                  repeat_add_columns(tibble(gcam.consumer=paste0("resid_rural_",unique(L144.income_shares$group)))) %>%
                  repeat_add_columns(tibble(year=HISTORICAL_YEARS[HISTORICAL_YEARS > 1974]))) %>%
      left_join_error_no_match(L101.pcgdp_thous90USD_province_Yh_gr, by=c("region"="province","year","gcam.consumer")) %>%
      rename(gdp_pc = pcGDP_thous90USD) %>%
      left_join_error_no_match(L101.Pop_thous_province_Yh_gr, by=c("year","gcam.consumer","region"="province")) %>%
      # use province-level habitable land to re-estimate the tot.den = province_pop/province_habitable
      left_join_error_no_match(L144.hab_land_flsp_china_fin, by="region") %>%
      left_join_error_no_match(L101.Pop_thous_province,by=c('region'='province','year')) %>%
      rename(pop_thous_province=pop) %>%
      mutate(tot.dens=round(pop_thous_province/area_thouskm2,2)) %>%
      mutate(flsp_pc_est = (`unadjust.satiation` + (-`land.density.param`*log(tot.dens))) * exp(-`b.param`
                                                                                                * exp(-`income.param` * log(gdp_pc)))) %>%
      mutate(flsp_est = flsp_pc_est * 1E-9 * pop_thous * 1E3)



    # Calculate the regional bias adder as the difference between observed (L144.flsp_bm2_province_bld) and estimated (L244.Floorspace_resid_est) data
    # Copy 2010 values for 2011-2012, 2015 values for 2013-2014
    L144.flsp_bm2_province_res_urb_2011_2015 <- L144.flsp_bm2_province_bld %>%
      filter(sector == "resid_urban") %>%
      filter(year == 2010) %>%
      select(-year) %>%
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
      filter(year == 2011 | year == 2012) %>%
      bind_rows(L144.flsp_bm2_province_bld %>%
                  filter(sector == "resid_urban") %>%
                  filter(year == 2015) %>%
                  select(-year) %>%
                  repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
                  filter(year == 2013 | year == 2014)) %>%
      bind_rows(L144.flsp_bm2_province_bld %>%
                  filter(sector == "resid_urban") %>%
                  filter(year == 2015))

    L144.flsp_bm2_province_res_urb <- L144.flsp_bm2_province_bld %>%
      filter(sector == "resid_urban") %>%
      filter(year > 1974 & year < 2011) %>%
      bind_rows(L144.flsp_bm2_province_res_urb_2011_2015)


    L144.flsp_bm2_province_res_rur_2011_2015 <- L144.flsp_bm2_province_bld %>%
      filter(sector == "resid_rural") %>%
      filter(year == 2010) %>%
      select(-year) %>%
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
      filter(year == 2011 | year == 2012) %>%
      bind_rows(L144.flsp_bm2_province_bld %>%
                  filter(sector == "resid_rural") %>%
                  filter(year == 2015) %>%
                  select(-year) %>%
                  repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
                  filter(year == 2013 | year == 2014)) %>%
      bind_rows(L144.flsp_bm2_province_bld %>%
                  filter(sector == "resid_rural") %>%
                  filter(year == 2015))

    L144.flsp_bm2_province_res_rur <- L144.flsp_bm2_province_bld %>%
      filter(sector == "resid_rural") %>%
      filter(year > 1974 & year < 2011) %>%
      bind_rows(L144.flsp_bm2_province_res_rur_2011_2015)

    # Allocate the adder equally across multiple consumers

    L244.Floorspace_resid_adder_province <- L244.Floorspace_resid_est %>%
      filter(grepl('resid_urban', gcam.consumer)) %>%
      group_by(region,year) %>%
      summarise(flsp_est = sum(flsp_est)) %>%
      ungroup() %>%
      left_join_error_no_match(L144.flsp_bm2_province_res_urb, by = c("year","region"="province")) %>%
      mutate(bias.adder = (value-flsp_est)/n_groups) %>%
      select(region,year,bias.adder) %>%
      repeat_add_columns(tibble(gcam.consumer=paste0("resid_urban_",unique(L144.income_shares$group)))) %>%
      bind_rows(L244.Floorspace_resid_est %>%
                  filter(grepl('resid_rural', gcam.consumer)) %>%
                  group_by(region,year) %>%
                  summarise(flsp_est = sum(flsp_est)) %>%
                  ungroup() %>%
                  left_join_error_no_match(L144.flsp_bm2_province_res_rur, by = c("year","region"="province")) %>%
                  mutate(bias.adder = (value-flsp_est)/n_groups) %>%
                  select(region,year,bias.adder) %>%
                  repeat_add_columns(tibble(gcam.consumer=paste0("resid_rural_",unique(L144.income_shares$group)))))

    # Combine observed data with the bias adder to obtain historical residential floorspace (BM2)
    L244.Floorspace_resid <- L244.Floorspace_resid_est %>%
      filter(grepl('resid_urban', gcam.consumer)) %>%
      select(region, gcam.consumer, year, flsp_est) %>%
      left_join_error_no_match(L244.Floorspace_resid_adder_province, by = c("region", "gcam.consumer","year")) %>%
      mutate(base.building.size = flsp_est + bias.adder) %>%
      mutate(nodeInput = "resid_urban",
             building.node.input = "resid_urban_building") %>%
      bind_rows(L244.Floorspace_resid_est %>%
                  filter(grepl('resid_rural', gcam.consumer)) %>%
                  select(region, gcam.consumer, year, flsp_est) %>%
                  left_join_error_no_match(L244.Floorspace_resid_adder_province, by = c("region", "gcam.consumer","year")) %>%
                  mutate(base.building.size = flsp_est + bias.adder) %>%
                  mutate(nodeInput = "resid_rural",
                         building.node.input = "resid_rural_building")) %>%
      select(region, gcam.consumer, nodeInput, building.node.input, year, base.building.size)


    # Get base per capita floorspace, which is used in future residential floorspace estimation (adjustment to avoid "destruction of buildings")
    # This is added to the final table that gathers all the parameters for the Gompertz function (L244.GompFnParam)
    L244.Base_pcFlsp <- L244.Floorspace_resid %>%
      filter(grepl('resid_urban', gcam.consumer)) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L101.Pop_thous_province_Yh_gr, by = c("region"="province", "gcam.consumer", "year")) %>%
      mutate(base.pcFlsp = (base.building.size * 1E9) / (pop_thous * 1E3)) %>%
      select(-base.building.size,-pop_thous) %>%
      bind_rows(L244.Floorspace_resid %>%
                  filter(grepl('resid_rural', gcam.consumer)) %>%
                  filter(year == MODEL_FINAL_BASE_YEAR) %>%
                  left_join_error_no_match(L101.Pop_thous_province_Yh_gr, by = c("region"="province", "gcam.consumer", "year")) %>%
                  mutate(base.pcFlsp = (base.building.size * 1E9) / (pop_thous * 1E3)) %>%
                  select(-base.building.size,-pop_thous))

    # Also need the regional bias adder per capita in the final calibration year to keep it to future periods
    # This is added to the final table that gathers all the parameters for the Gompertz function (L244.GompFnParam)
    L244.Flsp_BiasAdder <- L244.Floorspace_resid_est %>%
      filter(grepl('resid_urban', gcam.consumer)) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      group_by(region,year) %>%
      summarise(flsp_est=sum(flsp_est)) %>%
      ungroup() %>%
      left_join_error_no_match(L144.flsp_bm2_province_res_urb %>%
                                 filter(grepl('resid_urban', sector)), by = c("region"="province", "year")) %>%
      mutate(bm2_adder = round(value- flsp_est,9)) %>%
      select(-value,-flsp_est,-sector.match) %>%
      left_join_error_no_match(urban_pop_thous_prov_Yh, by = c("region"="province", "year")) %>%
      rename(pop_thous = pop) %>%
      mutate(bias.adjust.param = (bm2_adder*1E9)/(pop_thous*1E3)) %>%
      bind_rows(L244.Floorspace_resid_est %>%
                  filter(grepl('resid_rural', gcam.consumer)) %>%
                  filter(year == MODEL_FINAL_BASE_YEAR) %>%
                  group_by(region,year) %>%
                  summarise(flsp_est=sum(flsp_est)) %>%
                  ungroup() %>%
                  left_join_error_no_match(L144.flsp_bm2_province_res_rur %>%
                                             filter(grepl('resid_rural', sector)), by = c("region"="province", "year")) %>%
                  mutate(bm2_adder = round(value- flsp_est,9)) %>%
                  select(-value,-flsp_est,-sector.match) %>%
                  left_join_error_no_match(rural_pop_thous_prov_Yh, by = c("region"="province", "year")) %>%
                  rename(pop_thous = pop) %>%
                  mutate(bias.adjust.param = (bm2_adder*1E9)/(pop_thous*1E3)))

    # The following table puts together all the parameters that will be used in the estimation of future residential floorspace
    L244.GompFnParam_gcamchina <- L144.flsp_param_china %>%
      repeat_add_columns(tibble(gcam.consumer=paste0("resid_urban_",unique(L144.income_shares$group)))) %>%
      mutate(year=MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L244.Base_pcFlsp %>%
                                 filter(grepl('resid_urban', gcam.consumer)), by = c("region", "gcam.consumer", "year")) %>%
      left_join_error_no_match(L244.Flsp_BiasAdder %>%
                                 filter(grepl('resid_urban', sector)), by = c("region", "year")) %>%
      mutate(nodeInput = "resid_urban",
             building.node.input = "resid_urban_building") %>%
      bind_rows(L144.flsp_param_china %>%
                  repeat_add_columns(tibble(gcam.consumer=paste0("resid_rural_",unique(L144.income_shares$group)))) %>%
                  mutate(year=MODEL_FINAL_BASE_YEAR) %>%
                  left_join_error_no_match(L244.Base_pcFlsp %>%
                                             filter(grepl('resid_rural', gcam.consumer)), by = c("region", "gcam.consumer", "year")) %>%
                  left_join_error_no_match(L244.Flsp_BiasAdder %>%
                                             filter(grepl('resid_rural', sector)), by = c("region", "year")) %>%
                  mutate(nodeInput = "resid_rural",
                         building.node.input = "resid_rural_building")) %>%
      # delete "habitable.land" in LEVEL2_DATA_NAMES[["GompFnParam"]]
      select(LEVEL2_DATA_NAMES[["GompFnParam"]])

    # Rongqi 18/02/2024 end -------------


    # 2.Commercial floorspace ----
    L244.Floorspace_comm <- L144.flsp_bm2_province_bld %>%
      filter(sector == "comm") %>%
      filter(year > 1974) %>%
      rename(base.building.size = value,
             region = province,
             gcam.consumer = sector) %>%
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer") %>%
      select(LEVEL2_DATA_NAMES[["Floorspace"]])

    L244.Floorspace_full <- bind_rows(L244.Floorspace_resid, L244.Floorspace_comm)

    # Final output only has base years
    L244.Floorspace_gcamchina <- filter(L244.Floorspace_full, year %in% MODEL_BASE_YEARS) %>%
      mutate(base.building.size = round(base.building.size,energy.DIGITS_FLOORSPACE)) %>%
      mutate(base.building.size = if_else(region=="XZ", base.building.size*4, base.building.size))

    # # L244.DemandFunction_serv_gcamchina and L244.DemandFunction_flsp_gcamchina: demand function types
    # L244.DemandFunction_serv_gcamchina <- write_to_all_states(A44.demandFn_serv, LEVEL2_DATA_NAMES[["DemandFunction_serv"]])
    # L244.DemandFunction_flsp_gcamchina <- write_to_all_states(A44.demandFn_flsp, LEVEL2_DATA_NAMES[["DemandFunction_flsp"]])

    # L244.Satiation_flsp_gcamchina: Satiation levels assumed for floorspace
    L244.Satiation_flsp_gcamchina <- A44.satiation_flsp %>%
      gather(gcam.consumer, value, comm) %>%
      rename(region = province) %>%
      filter(region %in% gcamchina.PROVINCES_NOHKMC) %>%
      # Need to make sure that the satiation level is greater than the floorspace in the final base year
      left_join_error_no_match(L244.Floorspace_gcamchina %>%
                                 filter(year == max(MODEL_BASE_YEARS)), by = c("region", "gcam.consumer")) %>%
      left_join_error_no_match(L101.Pop_thous_province, by = c("region" = "province", "year")) %>%
      mutate(year = as.integer(year),
             # value.y = population
             pcflsp_mm2cap = base.building.size / pop,
             # Satiation level = must be greater than the observed value in the final calibration year, so if observed value is
             # greater than calculated, multiply observed by 1.001
             satiation.level = round(pmax(value * CONV_THOUS_BIL, pcflsp_mm2cap * 1.001), energy.DIGITS_SATIATION_ADDER)) %>%
      left_join_error_no_match(A44.gcam_consumer, by = c("gcam.consumer", "nodeInput", "building.node.input")) %>%
      select(LEVEL2_DATA_NAMES[["BldNodes"]], "satiation.level")

    # L244.SatiationImpedance_gcamchina: Calibrate satiation impedance per province.
    # sat_impedance = (-ln(2)/ln((satiation level - pcFlsp2015)/(satiation level - satiation adder))) * pcGDP2015

    L144.Satiation_impedance_gcamchina_pre <- L244.Satiation_flsp_gcamchina %>%
      mutate(year = max(MODEL_BASE_YEARS)) %>%
      # Add base floorspace
      left_join_error_no_match(L244.Floorspace_gcamchina,by=c("region","gcam.consumer","nodeInput","building.node.input","year")) %>%
      rename(flsp_bm2 = base.building.size) %>%
      # Add population to calculate floorspace per capita
      left_join_error_no_match(L101.Pop_thous_province %>% rename (region = province),
                               by=c("region","year")) %>%
      rename(pop_thous = pop) %>%
      mutate(flsp_pc = (flsp_bm2*1E9) / (pop_thous*1E3)) %>%
      # Add GDPpc
      left_join_error_no_match(L101.pcGDP_thous90usd_province %>% rename(region = province),
                               by=c("region","year")) %>%
      rename(pcGDP_thous90USD = pcGDP) %>%
      # Change units satiation level
      mutate(satiation.level = satiation.level * 1E6) %>%
      # Calculate satiation impedance
      mutate(`satiation-impedance` = (-log(2)/log((satiation.level - flsp_pc) / (satiation.level))) * pcGDP_thous90USD) %>%
      mutate(`satiation-impedance` = round(`satiation-impedance`, energy.DIGITS_SATIATION_IMPEDANCE)) %>%
      select(region,nodeInput, building.node.input, `satiation-impedance`)

    L244.Satiation_impedance_gcamchina <- L144.Satiation_impedance_gcamchina_pre %>%
      mutate(gcam.consumer = nodeInput) %>%
      select(LEVEL2_DATA_NAMES[["SatiationImpedance"]])


    # L244.SatiationAdder_gcamchina: Satiation adders in floorspace demand function
    # Required for shaping the future floorspace growth trajectories in each region
    # Match in the per-capita GDP, total floorspace, and population (for calculating per-capita floorspace)

    L244.SatiationAdder_gcamchina <- L244.Satiation_flsp_gcamchina %>%
      mutate(satiation.level = satiation.level * 1E6) %>%
      left_join_error_no_match(L244.Satiation_impedance_gcamchina,by = c("region", "gcam.consumer", "nodeInput", "building.node.input")) %>%
      mutate(year = max(MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(L244.Floorspace_gcamchina,by=c("region","year","gcam.consumer", "nodeInput", "building.node.input")) %>%
      rename(observed_flsp_bm2 = base.building.size) %>%
      left_join_error_no_match(L101.Pop_thous_province_Yh_gr %>% rename(region = province),
                               by = c("year", "region","gcam.consumer")) %>%
      mutate(observed_pcflsp = observed_flsp_bm2*1E9 / (pop_thous*1E3)) %>%
      left_join_error_no_match(L101.pcgdp_thous90USD_province_Yh_gr %>% rename(region = province),
                               by = c("year","region","gcam.consumer")) %>%
      mutate(est_pcflsp = satiation.level * (1-exp(-log(2)*pcGDP_thous90USD/`satiation-impedance`)),
             est_flsp_bm2 = (est_pcflsp*pop_thous*1E3) / 1E9) %>%
      group_by(region,nodeInput,building.node.input,year) %>%
      summarise(pop_thous = sum(pop_thous),
                est_flsp_bm2 = sum(est_flsp_bm2),
                observed_flsp_bm2 = sum(observed_flsp_bm2)) %>%
      ungroup() %>%
      mutate(est_flsp_bm2 = round(est_flsp_bm2,3),
             observed_flsp_bm2 = round(observed_flsp_bm2,3)) %>%
      mutate(satiation.adder = ((observed_flsp_bm2-est_flsp_bm2)*1E9) / (pop_thous*1E3)) %>%
      select(region,nodeInput,building.node.input,year,satiation.adder) %>%
      mutate(satiation.adder = round(satiation.adder,energy.DIGITS_SATIATION_ADDER)) %>%
      mutate(gcam.consumer = nodeInput) %>%
      select(LEVEL2_DATA_NAMES[["SatiationAdder"]])

    #================================================================
    # BUILDING ENERGY
    #================================================================
    # Heating and cooling degree days (thermal services only)
    # First, separate the thermal from the generic services. Generic services will be assumed to produce
    # internal gain energy, so anything in the internal gains assumptions table will be assumed generic
    generic_services <- unique(A44.globaltech_intgains$supplysector)
    thermal_services <- dplyr::setdiff(unique(A44.sector$supplysector), generic_services)

    # Downscale: HDDCDD at the provincial level, same as country
    L143.HDDCDD_scen_province <- L143.HDDCDD_scen_R_Y %>%
      filter(GCAM_region_ID == 11) %>%
      rename(Scen = SRES) %>%
      repeat_add_columns(tibble(province=paste0(gcamchina.PROVINCES_NOHKMC))) %>%
      select(-GCAM_region_ID)

    # L244.HDDCDD: Heating and cooling degree days by scenario
    L244.HDDCDD_scen_province <- L143.HDDCDD_scen_province %>%
      rename(region = province,
             degree.days = value)

    # Let's make a climate normal (historical average) for each region, using a selected interval of years
    # Don't want to just set one year, because we want average values for all regions
    L244.HDDCDD_normal_province <- L244.HDDCDD_scen_province %>%
      filter(year %in% seq(1981, 2000),
             # The AEO_2015 scenario changes this "normal climate" for each region,
             # which is not desirable since it does not incldue historical data
             # and is not the standard reference assumption.  Thus, we remove it
             # from this calculation.
             Scen != "AEO_2015") %>%
      group_by(region, variable) %>%
      summarise(degree.days = mean(degree.days)) %>%
      ungroup()

    # Subset the heating and cooling services, separately
    heating_services <- thermal_services[grepl("heating", thermal_services)]
    cooling_services <- thermal_services[grepl("cooling", thermal_services)]

    L244.HDDCDD_temp_pre <- tidyr::crossing(region = gcamchina.PROVINCES_NOHKMC, thermal.building.service.input = thermal_services) %>%
      # Add in gcam.consumer
      left_join_error_no_match(calibrated_techs_bld_china %>%
                                 # supplysector is the same as service
                                 select(supplysector, gcam.consumer = sector) %>%
                                 distinct(), by = c("thermal.building.service.input" = "supplysector"))

    L244.HDDCDD_temp <- L244.HDDCDD_temp_pre %>%
      filter(grepl('resid', gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer, c(gcam.consumer,group),sep="_") %>%
      bind_rows(L244.HDDCDD_temp_pre %>% filter(gcam.consumer=="comm")) %>%
      # Add in nodeInput and building.node.input
      left_join_error_no_match(A44.gcam_consumer, by = "gcam.consumer") %>%
      select(LEVEL2_DATA_NAMES[["BldNodes"]], thermal.building.service.input) %>%
      # Add in model years
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # Add HDD/CDD so that we can join with L244.HDDCDD_scen_state, remove at end
      mutate(variable = if_else(thermal.building.service.input %in% heating_services, "HDD", "CDD")) %>%
      # Add in degree days
      # L244.HDDCDD_scen_state has multiple scenarios, rows in this tbl_df are intended to be duplicated for each scenario
      # left_join_error_no_match throws an error when rows are duplicated (as intended), so left_join is used
      left_join(L244.HDDCDD_scen_province, by = c("region", "variable", "year")) %>%
      mutate(degree.days = round(degree.days, energy.DIGITS_HDDCDD))

    L244.HDDCDD_temp <- L244.HDDCDD_temp %>%
      filter(grepl('resid', nodeInput)) %>%
      separate(gcam.consumer,c("adj","group"),sep="_(?=[^_]*$)",remove=F) %>%
      unite(thermal.building.service.input, c("thermal.building.service.input","group"), sep="_") %>%
      select(-adj) %>%
      bind_rows(L244.HDDCDD_temp %>%
                  filter(!grepl('resid', nodeInput)))

    L244.HDDCDD_constdds_China <- L244.HDDCDD_temp %>%
      filter(Scen == "constdd") %>%
      select(-Scen, -GCM, -variable)

    L244.HDDCDD_A2_CCSM3x_China <- L244.HDDCDD_temp %>%
      filter(Scen == "A2", GCM == "CCSM3x") %>%
      select(-Scen, -GCM, -variable)

    L244.HDDCDD_A2_HadCM3_China <- L244.HDDCDD_temp %>%
      filter(Scen == "A2", GCM == "HadCM3") %>%
      select(-Scen, -GCM, -variable)

    L244.HDDCDD_B1_CCSM3x_China <- L244.HDDCDD_temp %>%
      filter(Scen == "B1", GCM == "CCSM3x") %>%
      select(-Scen, -GCM, -variable)

    L244.HDDCDD_B1_HadCM3_China <- L244.HDDCDD_temp %>%
      filter(Scen == "B1", GCM == "HadCM3") %>%
      select(-Scen, -GCM, -variable)

    # L244.ShellConductance_bld_gcamchina: Shell conductance (inverse of shell efficiency)
    L244.ShellConductance_bld_gcamchina <- A44.bld_shell_conductance %>%
      # Convert to long form
      gather_years() %>%
      # Interpolate to model years
      complete(gcam.consumer, year = c(year, MODEL_YEARS)) %>%
      group_by(gcam.consumer) %>%
      mutate(value = round(approx_fun(year, value), energy.DIGITS_EFFICIENCY)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      # Repeat for all states
      write_to_all_provinces(names = c(names(.), "region"), gcamchina.PROVINCES_NOHKMC) %>%
      # Add nodeInput and building.node.input
      left_join_error_no_match(A44.gcam_consumer %>%
                                 select(-gcam.consumer) %>%
                                 distinct() %>%
                                 # separate resid_urban and resid_rural
                                 # mutate(gcam.consumer= if_else(grepl("resid",nodeInput),"resid","comm")),
                                 mutate(gcam.consumer= nodeInput),
                               by = "gcam.consumer") %>%
      mutate(floor.to.surface.ratio = energy.FLOOR_TO_SURFACE_RATIO,
             shell.year = year) %>%
      # Rename columns
      rename(shell.conductance = value)



    # Rongqi add
    cons.groups <- unique(A44.gcam_consumer_resid$gcam.consumer)

    add.cg <- function(df){

      df.res <- df %>% filter(grepl("resid",supplysector))
      df.comm<-df %>% filter(grepl("comm",supplysector))

      df<- df.res %>%
        repeat_add_columns(tibble::tibble(cons.groups)) %>%
        separate(cons.groups,c("sector","cons.groups"),sep="_(?=[^_]*$)") %>%
        unite(supplysector,c(supplysector,cons.groups), sep="_") %>%
        select(-sector) %>%
        bind_rows(df.comm) %>%
        # Resid repeate it twice because urban and rural
        distinct()
      return(df)
    }

    A44.sector <- add.cg(A44.sector)
    A44.subsector_logit <- add.cg(A44.subsector_logit)
    A44.fuelprefElasticity <- add.cg(A44.fuelprefElasticity)
    A44.subsector_shrwt <- add.cg(A44.subsector_shrwt)
    A44.subsector_interp <- add.cg(A44.subsector_interp)
    A44.globaltech_shrwt <- add.cg(A44.globaltech_shrwt)
    # A44.globaltech_intgains<-add.cg(A44.globaltech_intgains)
    A44.globaltech_retirement <- add.cg(A44.globaltech_retirement)
    # A44.globaltech_eff<-add.cg(A44.globaltech_eff)
    # A44.globaltech_eff_avg<-add.cg(A44.globaltech_eff_avg)

    calibrated_techs_bld_china_adj <- calibrated_techs_bld_china %>%
      filter(grepl("resid",supplysector)) %>%
      repeat_add_columns(tibble::tibble(cons.groups)) %>%
      mutate(gcam.consumer=cons.groups) %>%
      separate(cons.groups,c("adj","cons.groups"),sep = "_(?=[^_]*$)") %>%
      unite(supplysector,c(supplysector,cons.groups), sep = "_", remove = F) %>%
      select(-adj,-cons.groups) %>%
      bind_rows(calibrated_techs_bld_china %>%
                  filter(grepl("comm",supplysector)) %>%
                  mutate(gcam.consumer=sector))


    # The remainder of the building-level parameters require information about the output of each service, which we do not have yet

    # L244.Supplysector_bld: Supplysector info for buildings
    L244.Supplysector_bld_gcamchina <- write_to_all_provinces(A44.sector, c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), gcamchina.PROVINCES_NOHKMC)

    # L244.FinalEnergyKeyword_bld: Supply sector keywords for detailed building sector
    L244.FinalEnergyKeyword_bld_gcamchina <- write_to_all_provinces(A44.sector, LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]], gcamchina.PROVINCES_NOHKMC)

    # L244.SubsectorLogit_bld: Subsector logit exponents of building sector
    L244.SubsectorLogit_bld_gcamchina <- write_to_all_provinces(A44.subsector_logit, c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), gcamchina.PROVINCES_NOHKMC)

    # 26/08/2024 Rongqi
    # L244.FuelPrefElast_bld_gcamchina: fuel preference elasticities of building energy use
    L244.FuelPrefElast_bld_gcamchina <- A44.fuelprefElasticity %>%
      mutate(year.fillout = min(MODEL_FUTURE_YEARS)) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["FuelPrefElast"]], gcamchina.PROVINCES_NOHKMC)


    # L244.SubsectorShrwt_bld and L244.SubsectorShrwtFllt_bld: Subsector shareweights of building sector
    if(any(!is.na(A44.subsector_shrwt$year))) {
      L244.SubsectorShrwt_bld_gcamchina <- write_to_all_provinces(A44.subsector_shrwt %>%
                                                                    filter(!is.na(year)), LEVEL2_DATA_NAMES[["SubsectorShrwt"]], gcamchina.PROVINCES_NOHKMC)
    }
    if(any(!is.na(A44.subsector_shrwt$year.fillout))) {
      L244.SubsectorShrwtFllt_bld_gcamchina <- write_to_all_provinces(A44.subsector_shrwt %>%
                                                                        filter(!is.na(year.fillout)), LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], gcamchina.PROVINCES_NOHKMC)
    }

    # L244.SubsectorInterp_bld and L244.SubsectorInterpTo_bld: Subsector shareweight interpolation of building sector
    if(any(is.na(A44.subsector_interp$to.value))) {
      L244.SubsectorInterp_bld_gcamchina <- write_to_all_provinces(A44.subsector_interp %>%
                                                                     filter(is.na(to.value)), LEVEL2_DATA_NAMES[["SubsectorInterp"]], gcamchina.PROVINCES_NOHKMC)
    }
    if(any(!is.na(A44.subsector_interp$to.value))) {
      L244.SubsectorInterpTo_bld_gcamchina <- write_to_all_provinces(A44.subsector_interp %>%
                                                                       filter(!is.na(to.value)), LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], gcamchina.PROVINCES_NOHKMC)
    }

    # L244.StubTech_bld_gcamchina: Identification of stub technologies for buildings
    L244.StubTech_bld_gcamchina <- add.cg(A44.globaltech_eff) %>%
      select(supplysector, subsector, technology) %>%
      distinct() %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["Tech"]], gcamchina.PROVINCES_NOHKMC) %>%
      rename(stub.technology = technology)

    # L244.GlobalTechEff_bld: Assumed efficiencies (all years) of buildings technologies
    L244.end_use_eff <- A44.globaltech_eff %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, MODEL_YEARS)) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(value = approx_fun(year, value)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      rename(efficiency = value)

    # Note - this code assumes that base-year efficiences are identical (should fix to copy over to make sure)
    L244.GlobalTechEff_bld_pre <- L244.end_use_eff %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechEff"]])

    # L244.StubTechMarket_bld_gcamchina: Specify market names for fuel inputs to all technologies in each province
    L244.StubTechMarket_bld_gcamchina <- add.cg(L244.end_use_eff) %>%
      mutate(market.name = gcamchina.REGION) %>%
      rename(stub.technology = technology) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["StubTechMarket"]], gcamchina.PROVINCES_NOHKMC) %>%
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

    # L244.StubTechCalInput_bld: Calibrated energy consumption by buildings technologies
    # Combine residential and commercial energy data


    L244.in_EJ_R_bld_serv_F_Yh <- L144.in_EJ_province_bld_F_U %>%
      filter(year %in% MODEL_YEARS) %>%
      mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT)) %>%
      rename(supplysector = service) %>%
      # Add subsector and energy.input
      # Rongqi 22/02/2024 modify calibrated_techs_bld_china.csv
      left_join_error_no_match(calibrated_techs_bld_china %>%
                                 select(sector, supplysector, fuel, subsector, minicam.energy.input) %>%
                                 distinct(), by = c("sector", "supplysector", "fuel")) %>%
      select(region = province, supplysector, subsector, minicam.energy.input, year, calibrated.value)

    # Shares allocated to partitioned technologies need to be computed first using efficiencies
    L244.globaltech_eff_prt <- A44.globaltech_eff %>%
      semi_join(A44.globaltech_eff_avg, by = c("supplysector", "subsector")) %>%
      # different with gcamusa.EFFICIENCY_PARTITION_YEAR = 2010
      # gcamchina.EFFICIENCY_PARTITION_YEAR = 2005
      filter(year == gcamchina.EFFICIENCY_PARTITION_YEAR) %>%
      select(supplysector, subsector, technology, efficiency = value)

    # Calculate technology shares using efficiency values
    L244.globaltech_shares <- A44.globaltech_eff_avg %>%
      # Adding specific technology efficiency to stock average efficiency
      left_join_error_no_match(L244.globaltech_eff_prt, by = c("supplysector", "subsector", "technology1" = "technology")) %>%
      rename(efficiency_tech1 = efficiency) %>%
      left_join_error_no_match(L244.globaltech_eff_prt, by = c("supplysector", "subsector", "technology2" = "technology")) %>%
      rename(efficiency_tech2 = efficiency) %>%
      # Calculate technology shares using stock average efficiency and individual technology efficiencies
      # Equation can be derived by solving following system of equations:
      # stockavg = efficiency_tech1 * share_tech1 + efficiency_tech2 * share_tech2
      # share_tech1 + share_tech2 = 1
      mutate(share_tech1 = (stockavg - efficiency_tech2) / (efficiency_tech1 - efficiency_tech2),
             share_tech2 = 1 - share_tech1) %>%
      # Keep only same names as A44.globaltech_shares and bind with A44.globaltech_shares
      select(names(A44.globaltech_shares)) %>%
      bind_rows(A44.globaltech_shares) %>%
      # Clunky, but we want only one technology and share value, currently have technology1, technology2, share1, share2
      gather(share_type, share, share_tech1, share_tech2) %>%
      gather(tech_type, technology, technology1, technology2) %>%
      # Filter for same technology and share number, then remove tech_type and share_type columns
      filter(substr(tech_type, nchar(tech_type), nchar(tech_type)) == substr(share_type, nchar(share_type), nchar(share_type))) %>%
      select(-tech_type, -share_type) %>%
      na.omit()

    # For calibration table, start with global tech efficiency table, repeat by provinces, and match in tech shares.
    L244.StubTechCalInput_bld_gcamchina_pre <- L244.GlobalTechEff_bld_pre %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      write_to_all_provinces(names = c(names(.), "region"), gcamchina.PROVINCES_NOHKMC) %>%
      rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) %>%
      left_join_error_no_match(L244.globaltech_shares, by = c("supplysector", "subsector", "stub.technology" = "technology")) %>%
      # Add energy by province/service/fuel
      left_join_error_no_match(L244.in_EJ_R_bld_serv_F_Yh, by = c("region", "supplysector", "subsector", "minicam.energy.input", "year")) %>%
      # calibrated.value = energy * share
      mutate(calibrated.value = round(share * calibrated.value, energy.DIGITS_CALOUTPUT),
             share.weight.year = year,
             calOutputValue = calibrated.value) %>%
      # Set subsector and technology shareweights
      set_subsector_shrwt() %>%
      mutate(tech.share.weight =  if_else(calibrated.value > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]])

    # L244.GlobalTechShrwt_bld_gcamchina: Default shareweights for global building technologies
    L244.GlobalTechShrwt_bld_gcamchina <- A44.globaltech_shrwt %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, share.weight)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      distinct() %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], share.weight)

    # L244.GlobalTechInterpTo_bld_gcamchina: Technology shareweight interpolation (selected techs only)
    L244.GlobalTechInterpTo_bld_gcamchina <- add.cg(A44.globaltech_interp) %>%
      set_years() %>%
      mutate(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechInterpTo"]]) %>%
      na.omit()

    # L244.GlobalTechCost_bld: Non-fuel costs of global building technologies
    L244.GlobalTechCost_bld_gcamchina <- add.cg(A44.globaltech_cost) %>%
      gather_years(value_col = "input.cost") %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_YEARS)) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(input.cost = approx_fun(year, input.cost)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      mutate(minicam.non.energy.input = "non-energy") %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]])


    # L244.GlobalTechSCurve_bld_gcamchina: Retirement rates for building technologies
    L244.GlobalTechSCurve_bld_gcamchina <- L244.GlobalTechCost_bld_gcamchina %>%
      filter(year %in% c(max(MODEL_BASE_YEARS), MODEL_FUTURE_YEARS),
             sector.name %in% A44.globaltech_retirement$supplysector) %>%
      # Add lifetimes and steepness
      left_join_error_no_match(A44.globaltech_retirement %>%
                                 distinct(), by = c("sector.name" = "supplysector")) %>%
      # Set steepness/halflife values to stock for base years, new for future years
      mutate(steepness = if_else(year == max(MODEL_BASE_YEARS), steepness_stock, steepness_new),
             half.life = if_else(year == max(MODEL_BASE_YEARS), half_life_stock, half_life_new)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechSCurve"]])

    # L244.GlobalTechIntGainOutputRatio_gcamchina: Output ratios of internal gain energy from non-thermal building services
    calibrated_techs_bld_china_consumer <- calibrated_techs_bld_china %>%
      select(gcam.consumer = sector, supplysector) %>%
      distinct()

    L244.GlobalTechIntGainOutputRatio_pre <- A44.globaltech_intgains %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # Add gcam.consumer (sector)
      left_join_error_no_match(calibrated_techs_bld_china_consumer, by = "supplysector") %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      # Add internal.gains.market.name
      left_join_error_no_match(A44.gcam_consumer %>%
                                 select(-gcam.consumer) %>%
                                 distinct() %>%
                                 mutate(gcam.consumer=nodeInput), by = "gcam.consumer") %>%
      # Add efficiency
      left_join_error_no_match(L244.GlobalTechEff_bld_pre, by = c("sector.name", "subsector.name", "technology", "year")) %>%
      mutate(internal.gains.output.ratio = round(input.ratio / efficiency, energy.DIGITS_EFFICIENCY)) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], internal.gains.output.ratio, internal.gains.market.name)

    # L244.GenericBaseService and L244.ThermalBaseService: Base year output of buildings services (per unit floorspace)
    # Base-service: Multiply energy consumption by efficiency for each technology, and aggregate by service

    L244.base_service_EJ_serv_fuel <- L244.StubTechCalInput_bld_gcamchina_pre %>%
      # Add in efficiency by technology
      left_join_error_no_match(L244.GlobalTechEff_bld_pre,
                               by = c("supplysector" = "sector.name", "subsector" = "subsector.name",
                                      "stub.technology" = "technology", "year", "minicam.energy.input")) %>%
      # Calculate base.service = calibrated.value(energy) * efficiency
      mutate(base.service = round(calibrated.value * efficiency, energy.DIGITS_CALOUTPUT)) %>%
      group_by(region, supplysector, subsector, year, base.service) %>%
      summarise(base.service = sum(base.service)) %>%
      ungroup()

    L244.base_service_EJ_serv <- L244.base_service_EJ_serv_fuel %>%
      group_by(region,supplysector, year) %>%
      summarise(base.service=sum(base.service)) %>%
      ungroup()

    L244.base_service <- L244.base_service_EJ_serv %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      # Add gcam.consumer (sector)
      left_join_error_no_match(calibrated_techs_bld_china_consumer, by = "supplysector") %>%
      # Add nodeInput and building.node.input
      left_join_error_no_match(A44.gcam_consumer %>%
                                 select(-gcam.consumer) %>%
                                 distinct() %>%
                                 mutate(gcam.consumer=nodeInput), by = "gcam.consumer") %>%
      select(LEVEL2_DATA_NAMES[["BldNodes"]], building.service.input = supplysector, year, base.service)


    L244.GenericBaseService_gcamchina <- L244.base_service %>%
      filter(building.service.input %in% generic_services) %>%
      complete(nesting(region,year), building.service.input = c(building.service.input, generic_services)) %>%
      # mutate(gcam.consumer = if_else(grepl("resid",building.service.input),"resid","comm"),
      # nodeInput = if_else(grepl("resid",building.service.input),"resid","comm"),
      # building.node.input = if_else(grepl("resid",building.service.input),"resid_building","comm_building")) %>%
      select(LEVEL2_DATA_NAMES[["GenericBaseService"]])

    L244.ThermalBaseService_gcamchina <- L244.base_service %>%
      filter(building.service.input %in% thermal_services) %>%
      rename(thermal.building.service.input = building.service.input) %>%
      complete(nesting(region,year), thermal.building.service.input = c(thermal.building.service.input, thermal_services)) %>%
      # mutate(gcam.consumer = if_else(grepl("resid",thermal.building.service.input),"resid","comm"),
      # nodeInput = if_else(grepl("resid",thermal.building.service.input),"resid","comm"),
      # building.node.input = if_else(grepl("resid",thermal.building.service.input),"resid_building","comm_building")) %>%
      select(LEVEL2_DATA_NAMES[["ThermalBaseService"]])

    # YZ 2022.9.20 this is what's done in the global version (L144 done in LA144.building_det_en and L244 in the corresponding L244.building_det)
    # L144.in_EJ_R_bld_serv_F_Yh %>%
    #   left_join_error_no_match(L144.end_use_eff_2f, by = c("GCAM_region_ID", "sector", "fuel", "service", "year")) %>%
    #   mutate(value = value * value_eff) %>%
    #   select(GCAM_region_ID, sector, fuel, service, year, value)->
    #   L144.base_service_EJ_serv_fuel
    #
    # L244.base_service <- L244.in_EJ_R_bld_serv_F_Yh %>%
    #   group_by(region,supplysector,year) %>%
    #   summarise(base.service=sum(calibrated.value)) %>%
    #   ungroup() %>%
    #   filter(year %in% MODEL_BASE_YEARS) %>%
    #   left_join_keep_first_only(calibrated_techs_bld_usa, by = "supplysector") %>%
    #   left_join_error_no_match(calibrated_techs_bld_usa_consumer, by = "supplysector") %>%
    #   left_join_error_no_match(A44.gcam_consumer %>%
    #                              select(-gcam.consumer) %>%
    #                              distinct() %>%
    #                              mutate(gcam.consumer=if_else(grepl("resid",nodeInput),"resid","comm")), by = "gcam.consumer") %>%
    #   select(LEVEL2_DATA_NAMES[["BldNodes"]], building.service.input = supplysector, year, base.service)
    # -----^^^----

    # Separate thermal and generic services into separate tables with different ID strings
    # L244.GenericBaseService_gcamusa <- L244.base_service %>%
    #   filter(supplysector %in% generic_services) %>%
    #   rename(building.service.input = supplysector) %>%
    #   select(LEVEL2_DATA_NAMES[["GenericBaseService"]])
    #
    # L244.ThermalBaseService_gcamusa <- L244.base_service %>%
    #   filter(supplysector %in% thermal_services) %>%
    #   rename(thermal.building.service.input = supplysector) %>%
    #   select(LEVEL2_DATA_NAMES[["ThermalBaseService"]])

    # L244.GenericServiceSatiation_gcamusa: Satiation levels assumed for non-thermal building services
    # Just multiply the base-service by an exogenous multiplier
    L244.GenericServiceSatiation_gcamchina <- L244.GenericBaseService_gcamchina %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      # Add floorspace
      left_join_error_no_match(L244.Floorspace_gcamchina %>%
                                 group_by(region,nodeInput,building.node.input,year) %>%
                                 summarise(base.building.size=sum(base.building.size)) %>%
                                 ungroup() %>%
                                 mutate(gcam.consumer = nodeInput),
                               by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "year")) %>%
      # Add multiplier
      left_join_error_no_match(A44.demand_satiation_mult, by = c("building.service.input" = "supplysector")) %>%
      # Satiation level = service per floorspace * multiplier
      mutate(satiation.level = round(base.service / base.building.size * multiplier, energy.DIGITS_COEFFICIENT)) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]])

    # L244.ThermalServiceSatiation: Satiation levels assumed for thermal building services
    L244.ThermalServiceSatiation_gcamchina <- L244.ThermalBaseService_gcamchina %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      # Add floorspace
      left_join_error_no_match(L244.Floorspace_gcamchina %>%
                                 group_by(region,nodeInput,building.node.input,year) %>%
                                 summarise(base.building.size=sum(base.building.size)) %>%
                                 ungroup() %>%
                                 mutate(gcam.consumer = nodeInput),
                               by = c(LEVEL2_DATA_NAMES[["BldNodes"]], "year")) %>%
      # Add multiplier
      left_join_error_no_match(A44.demand_satiation_mult, by = c("thermal.building.service.input" = "supplysector")) %>%
      # Satiation level = service per floorspace * multiplier
      mutate(satiation.level = round(base.service / base.building.size * multiplier, energy.DIGITS_COEFFICIENT)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceSatiation"]])

    # L244.Intgains_scalar: Scalers relating internal gain energy to increased/reduced cooling/heating demands
    variable <- c("HDD", "CDD")
    scalar <- c(energy.INTERNAL_GAINS_SCALAR_CHINA_H, energy.INTERNAL_GAINS_SCALAR_CHINA_C)
    DDnorm <- c(gcamchina.BASE_HDD_CHINA, gcamchina.BASE_CDD_CHINA)
    CHINA.base.scalar <- tibble(variable, scalar, DDnorm)
    threshold_HDD <- 500

    L244.Intgains_scalar_gcamchina <- L244.ThermalServiceSatiation_gcamchina %>%
      # Assign HDD or CDD
      mutate(variable = if_else(thermal.building.service.input %in% heating_services, "HDD", "CDD")) %>%
      # Add DDnorm & scalar
      left_join_error_no_match(CHINA.base.scalar, by = "variable") %>%
      # Add degree days
      left_join_error_no_match(L244.HDDCDD_normal_province, by = c("region", "variable")) %>%
      mutate(internal.gains.scalar = round(scalar * degree.days / DDnorm, energy.DIGITS_HDDCDD),
             # Prevent very warm places from having negative heating demands, using exogenous threshold
             internal.gains.scalar = if_else(variable == "HDD" & degree.days < threshold_HDD, 0, internal.gains.scalar)) %>%
      distinct() %>%
      select(LEVEL2_DATA_NAMES[["Intgains_scalar"]])


    # YZ add ----

    # Need to remove any services (supplysectors and building-service-inputs) and intgains trial markets for services that don't exist in any years
    # L244.DeleteThermalService and L244.DeleteGenericService: Removing non-existent services, likely related to 0 HDD or CDD

    L244.DeleteThermalService_gcamchina_pre <- L244.ThermalBaseService_gcamchina %>%
      group_by(region, gcam.consumer, nodeInput, building.node.input, thermal.building.service.input) %>%
      summarise(base.service = max(base.service)) %>%
      ungroup()

    L244.DeleteThermalService_gcamchina <- calibrated_techs_bld_china %>%
      select(supplysector) %>%
      distinct() %>%
      filter(supplysector %in% thermal_services) %>%
      repeat_add_columns(tibble(region = unique(province_names_mappings$province))) %>%
      select(region, supplysector) %>%
      rename(service = supplysector) %>%
      left_join(L244.DeleteThermalService_gcamchina_pre %>% select(region,service=thermal.building.service.input,base.service), by = c("region", "service")) %>%
      #replace_na(list(base.service = 0)) %>%
      filter(complete.cases(.)) %>%
      # adjust Eastern Africa to not delete modern services
      # mutate(base.service = if_else(grepl("Africa",region) & service == "resid heating modern",1e-9,base.service)) %>%
      filter(base.service == 0) %>%
      select(-base.service) %>%
      rename(supplysector = service) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      mutate(gcam.consumer = case_when(
        grepl("resid_rural", supplysector) ~ "resid_rural",
        grepl("resid_urban", supplysector) ~ "resid_urban",
        grepl("comm", supplysector) ~ "comm"),
        nodeInput = gcam.consumer,
        building.node.input = paste0(nodeInput,"_building")) %>%
      unite(gcam.consumer, c(gcam.consumer,group),sep="_") %>%
      separate(gcam.consumer, c("adj","group"),sep="_(?=[^_]*$)",remove = F) %>%
      # adjust commercial
      mutate(gcam.consumer = if_else(grepl("comm",gcam.consumer),"comm",gcam.consumer)) %>%
      mutate(thermal.building.service.input = supplysector,
             supplysector =if_else(grepl("resid",gcam.consumer),paste0(supplysector,"_",group),supplysector),
             thermal.building.service.input = if_else(grepl("resid",gcam.consumer) ,paste0(thermal.building.service.input,"_",group),thermal.building.service.input)) %>%
      select(LEVEL2_DATA_NAMES[["DeleteThermalService"]]) %>%
      filter(!is.na(region))


    # ------------------------------------
    # Dec 2024, Rongqi
    # Due to the lackness of modern service and traditional service (coal and TradBio), drop this part
    # change traditional service to modern service's subsector
    # The demand for traditional fuels (coal and TradBio) is an inverse function (service demand as inverse of service affordability)
    # The parameters to define the functional form are estimated in the following lines:

    # # First, estimate the parameters for the function to estimate coal demand:
    # serv_coal <- L244.base_service_EJ_serv_fuel %>%
    #   rename(en_EJ = base.service) %>%
    #   filter(grepl("resid",supplysector),
    #          subsector == "coal") %>%
    #   gcamdata::left_join_error_no_match(L101.pcGDP_thous90usd_province,
    #                                      by = c("region"="province", "year")) %>%
    #   rename(pcgdp_thous = pcGDP) %>%
    #   select(region,fuel=subsector,service=supplysector,year,pcgdp_thous,en_EJ) %>%
    #   arrange(en_EJ) %>%
    #   left_join(L144.prices_bld_gcamchina %>% rename(service = sector,price=value), by = c("region", "year","service")) %>%
    #   replace_na(list(price = 1)) %>%
    #   mutate(afford=(pcgdp_thous*1000/def9075) / price)
    #
    # # yz this formula formula.coal<- "en_EJ~A/(afford+k)"
    # # does not work (showing "Error in nls() : singular gradient")
    # # because the model is not identifiable, that is, there is more than one solution to the model
    # # Therefore, we reduce the model parameters (ignore k or set k=0) for USA coal general fit across sercives
    #
    # # formula.coal<- "en_EJ~A/(afford+k)"
    # # start.value.coal<-c(A = 1,k=0.5)
    #
    # formula.coal <- "en_EJ~A/(afford)"
    # start.value.coal <- c(A = 1)
    #
    # fit_coal <- nls(formula.coal, serv_coal, start.value.coal)
    # A_coal <- coef(fit_coal)[1]
    # k_coal <- 0
    #
    # # Same for traditional biomass:
    # serv_TradBio <- L244.base_service_EJ_serv_fuel %>%
    #   rename(en_EJ = base.service) %>%
    #   filter(grepl("resid",supplysector),
    #          subsector == "traditional biomass") %>%
    #   gcamdata::left_join_error_no_match(L101.pcGDP_thous90usd_province,
    #                                      by = c("region"="province", "year")) %>%
    #   rename(pcgdp_thous = pcGDP) %>%
    #   select(region,fuel=subsector,service=supplysector,year,pcgdp_thous,en_EJ) %>%
    #   arrange(en_EJ) %>%
    #   left_join(L144.prices_bld_gcamchina %>% rename(service = sector,price=value), by = c("region", "year","service")) %>%
    #   replace_na(list(price = 1)) %>%
    #   mutate(afford=(pcgdp_thous*1000/def9075) / price)
    #
    # formula.tradBio<- "en_EJ~x/(afford+y)"
    # start.value.tradBio<-c(x = 10,y = 10)
    #
    # fit_tradBio<-nls(formula.tradBio, serv_TradBio, start.value.tradBio)
    # x_TradBio<-coef(fit_tradBio)[1]
    # y_TradBio<-coef(fit_tradBio)[2]
    #
    # end of adding---


    #------------------------------------------------------
    # In order to make the function flexible to the implementation of multiple consumers, the satiation impedance (mu) and the calibration coefficent (k)
    # were calibrated in the DS in the global version.
    # Considering that GCAM-CHINA uses the same building_service function,this needs to be extended to this module.
    # Therefore, here we create L244.ThermalServiceImpedance_gcamchina, L244.GenericServiceImpedance_gcamchina,
    # L244.GenericServiceCoef_gcamchina, and L244.ThermalServiceCoef_gcamchina

    # 1-Generic services
    L244.GenericServiceImpedance_allvars_gcamchina <- L244.GenericServiceSatiation_gcamchina %>%
      left_join(L244.base_service %>%  filter(year==MODEL_FINAL_BASE_YEAR, building.service.input %in% generic_services),
                by=c("region","gcam.consumer","nodeInput","building.node.input","building.service.input")) %>%
      rename(base_service_EJ = base.service) %>%
      left_join_error_no_match(L244.Floorspace_gcamchina %>%
                                 group_by(region,nodeInput,building.node.input,year) %>%
                                 summarise(base.building.size=sum(base.building.size)) %>%
                                 ungroup() %>%
                                 mutate(gcam.consumer = case_when(
                                   grepl("resid_rural", nodeInput) ~ "resid_rural",
                                   grepl("resid_urban", nodeInput) ~ "resid_urban",
                                   grepl("comm", nodeInput) ~ "comm")),
                               by=c("region","year","gcam.consumer","nodeInput","building.node.input")) %>%
      mutate(base_serv_flsp=base_service_EJ / base.building.size) %>%
      select(-base_service_EJ,-base.building.size) %>%
      # Add pcGDP
      left_join_error_no_match(L101.pcGDP_thous90usd_province %>% rename(region = province),
                               by=c("year","region")) %>%
      rename(pcGDP_thous90USD = pcGDP) %>%
      # Add service prices: At this point, we read the calibrated prices from GCAM-China v6.0
      left_join_error_no_match(L144.prices_bld_gcamchina  %>%
                                 rename(building.service.input = sector) %>%
                                 filter(building.service.input %in% generic_services, year == MODEL_FINAL_BASE_YEAR),
                               by=c("region","year","building.service.input")) %>%
      rename(price = value) %>%
      mutate(`satiation-impedance` = (log(2)*((pcGDP_thous90USD*1000/def9075)/price)) / log((satiation.level)/(satiation.level-base_serv_flsp))) %>%
      # Check with an adder to be 0!!!!
      rename(observed_base_serv_perflsp = base_serv_flsp) %>%
      mutate(thermal_load = 1) %>%
      mutate(afford=(pcGDP_thous90USD*1000/def9075) / price) %>%
      mutate(serv_density = satiation.level * (1-exp((-log(2)/`satiation-impedance`) * afford))) %>%
      mutate(serv_density = if_else(grepl("coal",building.service.input),observed_base_serv_perflsp,serv_density),
             serv_density = if_else(grepl("TradBio",building.service.input),observed_base_serv_perflsp,serv_density)) %>%
      mutate(coef = observed_base_serv_perflsp / serv_density*thermal_load) %>%
      mutate(est_base_serv_perflsp = coef * thermal_load * serv_density) %>%
      mutate(bias.adder = round(est_base_serv_perflsp-observed_base_serv_perflsp,energy.DIGITS_BIAS_ADDER))


    L244.GenericServiceImpedance_gcamchina <- L244.GenericServiceImpedance_allvars_gcamchina %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceImpedance"]]) %>%
      filter(grepl("resid",gcam.consumer),
             grepl("modern",building.service.input)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.GenericServiceImpedance_allvars_gcamchina %>%
                  select(LEVEL2_DATA_NAMES[["GenericServiceImpedance"]]) %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      mutate(`satiation-impedance` = round(`satiation-impedance`, energy.DIGITS_SATIATION_IMPEDANCE))

    L244.GenericServiceCoef_gcamchina <- L244.GenericServiceImpedance_allvars_gcamchina %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceCoef"]]) %>%
      filter(grepl("resid",gcam.consumer),
             grepl("modern",building.service.input)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.GenericServiceImpedance_allvars_gcamchina %>%
                  select(LEVEL2_DATA_NAMES[["GenericServiceCoef"]]) %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      mutate(coef = round(coef,energy.DIGITS_COEFFICIENT))


    # yz this is not in the global chunk
    # L244.GenericServiceAdder_gcamusa<-L244.GenericServiceImpedance_allvars_gcamusa %>%
    #   select(LEVEL2_DATA_NAMES[["GenericServiceAdder"]]) %>%
    #   mutate(bias.adder = round(bias.adder,energy.DIGITS_BIAS_ADDER))


    # 2-Thermal services
    # First calculate internal gains
    L244.internal_gains_gcamchina <- L244.StubTechCalInput_bld_gcamchina_pre %>%
      # Add in efficiency by technology
      left_join_error_no_match(L244.GlobalTechEff_bld_pre %>%
                                 rename(stub.technology = technology,
                                        supplysector = sector.name,
                                        subsector = subsector.name),
                               by = c("supplysector", "subsector" ,
                                      "stub.technology", "year", "minicam.energy.input")) %>%
      # Calculate base.service = calibrated.value(energy) * efficiency
      mutate(base.service = round(calibrated.value * efficiency, energy.DIGITS_CALOUTPUT)) %>%
      # use left join because not all services produce internal gains
      left_join(L244.GlobalTechIntGainOutputRatio_pre,
                by = c("supplysector" = "sector.name", "subsector" = "subsector.name" ,
                       "stub.technology"="technology", "year")) %>%
      filter(complete.cases(.)) %>%
      mutate(int_gains = base.service*internal.gains.output.ratio) %>%
      mutate(gcam.consumer = case_when(
        grepl("resid_rural", supplysector) ~ "resid_rural",
        grepl("resid_urban", supplysector) ~ "resid_urban",
        grepl("comm", supplysector) ~ "comm")) %>%
      group_by(region,gcam.consumer,year) %>%
      summarise(int_gains = sum(int_gains)) %>%
      ungroup() %>%
      rename(intGains_EJ= int_gains)


    L244.ThermalServiceImpedance_allvars_gcamchina <- L244.ThermalServiceSatiation_gcamchina %>%
      filter(satiation.level!=0) %>%
      left_join_error_no_match(L244.base_service %>%  filter(year==MODEL_FINAL_BASE_YEAR, building.service.input %in% thermal_services)
                               %>% rename(thermal.building.service.input = building.service.input),
                               by=c("region","gcam.consumer","nodeInput","building.node.input","thermal.building.service.input")) %>%
      rename(base_service_EJ = base.service) %>%
      left_join_error_no_match(L244.Floorspace_gcamchina %>%
                                 group_by(region,nodeInput,building.node.input,year) %>%
                                 summarise(base.building.size=sum(base.building.size)) %>%
                                 ungroup() %>%
                                 mutate(gcam.consumer = case_when(
                                   grepl("resid_rural", nodeInput) ~ "resid_rural",
                                   grepl("resid_urban", nodeInput) ~ "resid_urban",
                                   grepl("comm", nodeInput) ~ "comm")),
                               by=c("region","year","gcam.consumer","nodeInput","building.node.input")) %>%
      mutate(base_serv_flsp=base_service_EJ / base.building.size) %>%
      select(-base_service_EJ) %>%
      # Add pcGDP
      left_join_error_no_match(L101.pcGDP_thous90usd_province %>% rename(region = province),
                               by=c("year","region")) %>%
      rename(pcGDP_thous90USD = pcGDP) %>%
      # Add service prices: At this point, we read the calibrated prices from GCAM v5.4
      left_join_error_no_match(L144.prices_bld_gcamchina  %>%
                                 rename(thermal.building.service.input = sector) %>%
                                 filter(thermal.building.service.input %in% thermal_services, year == MODEL_FINAL_BASE_YEAR),
                               by=c("region","year","thermal.building.service.input")) %>%
      rename(price = value) %>%
      filter(complete.cases(.)) %>%
      mutate(`satiation-impedance` = (log(2)*((pcGDP_thous90USD*1000/def9075)/price)) / log((satiation.level)/(satiation.level-base_serv_flsp))) %>%
      mutate(dd=if_else(grepl("cooling",thermal.building.service.input),"CDD","HDD")) %>%
      left_join_error_no_match(L244.HDDCDD_temp %>% filter(year == MODEL_FINAL_BASE_YEAR,
                                                           # GCM is different with GCAM-USA(hist)
                                                           GCM == "no_GCM", Scen == "constdd") %>%
                                 rename(dd = variable) %>%
                                 select(dd, year, degree.days, region) %>%
                                 distinct(),
                               by=c("region","year","dd")) %>%
      left_join_error_no_match(L244.ShellConductance_bld_gcamchina %>% select(-shell.year) %>% filter(year == MODEL_FINAL_BASE_YEAR),
                               by=c("region","year","gcam.consumer","nodeInput","building.node.input")) %>%
      left_join_error_no_match(L244.internal_gains_gcamchina %>% filter(year == MODEL_FINAL_BASE_YEAR), by=c("region","year","gcam.consumer")) %>%
      left_join_error_no_match(L244.Intgains_scalar_gcamchina,by = c("region","gcam.consumer","nodeInput",
                                                                     "building.node.input","thermal.building.service.input")) %>%
      mutate(intGains_EJ_serv = intGains_EJ / base.building.size) %>%
      mutate(thermal_load = degree.days * shell.conductance * floor.to.surface.ratio + internal.gains.scalar*intGains_EJ_serv) %>%
      select(-base.building.size) %>%
      rename(observed_base_serv_perflsp = base_serv_flsp) %>%
      mutate(afford = (pcGDP_thous90USD*1000/def9075) / price) %>%
      mutate(serv_density=satiation.level * (1-exp((-log(2)/`satiation-impedance`)*afford))) %>%
      mutate(serv_density = if_else(grepl("coal",thermal.building.service.input),observed_base_serv_perflsp,serv_density),
             serv_density = if_else(grepl("TradBio",thermal.building.service.input),observed_base_serv_perflsp,serv_density)) %>%
      mutate(coef = observed_base_serv_perflsp / (serv_density*thermal_load)) %>%
      mutate(est_base_serv_perflsp = coef * thermal_load * serv_density) %>%
      mutate(bias.adder = round(est_base_serv_perflsp-observed_base_serv_perflsp,energy.DIGITS_BIAS_ADDER))

    L244.ThermalServiceImpedance_gcamchina <- L244.ThermalServiceImpedance_allvars_gcamchina %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceImpedance"]]) %>%
      filter(grepl("resid",gcam.consumer),
             grepl("modern",thermal.building.service.input)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.ThermalServiceImpedance_allvars_gcamchina %>%
                  select(LEVEL2_DATA_NAMES[["ThermalServiceImpedance"]]) %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      mutate(`satiation-impedance`  = round(`satiation-impedance`, energy.DIGITS_SATIATION_IMPEDANCE))

    L244.ThermalServiceCoef_gcamchina <- L244.ThermalServiceImpedance_allvars_gcamchina %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceCoef"]]) %>%
      filter(grepl("resid",gcam.consumer),
             grepl("modern",thermal.building.service.input)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.ThermalServiceImpedance_allvars_gcamchina %>%
                  select(LEVEL2_DATA_NAMES[["ThermalServiceCoef"]]) %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      # adjust coal and tradbio
      mutate(coef = if_else(grepl("coal",thermal.building.service.input),1,coef),
             coef = if_else(grepl("TradBio",thermal.building.service.input),1,coef)) %>%
      mutate(coef = round(coef,energy.DIGITS_COEFFICIENT))


    # L244.GenericBaseService adjusted
    L244.GenericBaseService_gcamchina_pre <- L244.GenericBaseService_gcamchina %>%
      left_join_error_no_match(L101.pcGDP_thous90usd_province,
                               by = c("year","region"="province")) %>%
      rename(pcGDP_thous90USD = pcGDP) %>%
      left_join(L144.prices_bld_gcamchina  %>%
                  rename(building.service.input=sector,price=value)
                ,by = c("region","year","building.service.input")) %>%
      replace_na(list(price = 0)) %>%
      left_join(L244.GenericServiceImpedance_allvars_gcamchina %>%
                  select(region,gcam.consumer,nodeInput,building.service.input,satiation.level,`satiation-impedance`)
                ,by=c("region","gcam.consumer","nodeInput","building.service.input")) %>%
      select(LEVEL2_DATA_NAMES[["GenericBaseService"]],satiation.level,`satiation-impedance`,pcGDP_thous90USD,price)

    L244.GenericShares_gcamchina_pre <- L244.GenericBaseService_gcamchina_pre %>%
      filter(grepl("resid",gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.GenericBaseService_gcamchina_pre %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      left_join_error_no_match(L101.Pop_thous_province_Yh_gr, by=c("year","region"="province","gcam.consumer")) %>%
      rename(pop_thous_gr = pop_thous) %>%
      left_join_error_no_match(L101.Pop_thous_province, by=c("year","region"="province")) %>%
      rename(pop_thous = pop) %>%
      left_join_error_no_match(L244.SubregionalShares_gcamchina %>%
                                 select(-inc.year.fillout) %>%
                                 rename(year = pop.year.fillout)
                               , by=c("region","gcam.consumer","year")) %>%
      mutate(pcGDP_thous90USD_gr = (pcGDP_thous90USD * 1E3 * pop_thous * 1E3 * subregional.income.share) / (pop_thous_gr * 1E3 * 1E3)) %>%
      mutate(afford = (pcGDP_thous90USD_gr*1000/def9075) / price) %>%
      mutate(afford = if_else(is.infinite(afford),0,afford)) %>%
      left_join_error_no_match(bind_rows(L244.Floorspace_resid,L244.Floorspace_comm),
                               by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "year")) %>%
      mutate(serv=(satiation.level * (1-exp((-log(2)/`satiation-impedance`)*afford))) * base.building.size) %>%
      # Adjust coal and TradBio ( no coal and tradBio in gcam-usa's generic services)
      # mutate(serv = if_else(grepl("coal",building.service.input),coef(fit_coal)[1] / (afford),serv),
      #        serv = if_else(grepl("TradBio",building.service.input),coef(fit_tradBio)[1] / (afford + coef(fit_tradBio)[2]),serv)) %>%
      mutate(serv = if_else(afford == 0, 0, serv))


    # Calculate subtotals (for shares)
    L244.GenericShares_gcamchina_pre_subt <- L244.GenericShares_gcamchina_pre %>%
      group_by(region,year,building.service.input) %>%
      summarise(serv = sum(serv)) %>%
      ungroup() %>%
      rename(serv_aggReg = serv)

    # Merge the subtotals to the estimated values to calculate %shares for each consumer group, in each region and period
    L244.GenericShares_gcamchina <- L244.GenericShares_gcamchina_pre %>%
      left_join_error_no_match(L244.GenericShares_gcamchina_pre_subt
                               , by=c("region","building.service.input","year")) %>%
      mutate(gen_share = serv / serv_aggReg) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,building.service.input,year,gen_share) %>%
      replace_na(list(gen_share = 0)) %>%
      # Check
      group_by(region,nodeInput,building.node.input,building.service.input,year) %>%
      mutate(agg_gen_share = sum(gen_share)) %>%
      ungroup()


    L244.GenericBaseService_gcamchina <- L244.GenericBaseService_gcamchina_pre %>%
      select(region,gcam.consumer,nodeInput,building.node.input,building.service.input,year, base.service) %>%
      filter(grepl("resid",nodeInput)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.GenericBaseService_gcamchina_pre %>%
                  select(region,gcam.consumer,nodeInput,building.node.input,building.service.input,year,base.service) %>%
                  filter(grepl("comm",nodeInput))) %>%
      left_join_error_no_match(L244.GenericShares_gcamchina %>% select(-agg_gen_share)
                               , by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "building.service.input","year")) %>%
      mutate(base.service = base.service * gen_share) %>%
      select(-gen_share) %>%
      mutate(base.service = round(base.service, energy.DIGITS_SERVICE))


    # L244.ThermalBaseService adjusted
    L244.ThermalBaseService_gcamchina_pre <- L244.ThermalBaseService_gcamchina %>%
      left_join_error_no_match(L101.pcGDP_thous90usd_province,
                               by=c("year","region"="province")) %>%
      rename(pcGDP_thous90USD = pcGDP) %>%
      # Add service prices: At this point, we read the calibrated prices from GCAM v5.4 ("L144.prices_bld")
      left_join(L144.prices_bld_gcamchina  %>%
                  rename(thermal.building.service.input = sector,price=value) %>%
                  filter(thermal.building.service.input %in% thermal_services),
                by=c("year","thermal.building.service.input","region")) %>%
      left_join(L244.ThermalServiceImpedance_allvars_gcamchina %>%
                  select(region, gcam.consumer, nodeInput, thermal.building.service.input, thermal_load, satiation.level, `satiation-impedance`) %>%
                  repeat_add_columns(tibble(year=MODEL_BASE_YEARS))
                ,by=c("region","gcam.consumer","nodeInput","thermal.building.service.input","year")) %>%
      select(LEVEL2_DATA_NAMES[["ThermalBaseService"]], satiation.level, `satiation-impedance`, thermal_load, pcGDP_thous90USD, price)


    L244.ThermalShares_gcamchina_pre <- L244.ThermalBaseService_gcamchina_pre %>%
      select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,year,base.service,satiation.level,thermal_load,pcGDP_thous90USD,price,`satiation-impedance`) %>%
      filter(grepl("resid",gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.ThermalBaseService_gcamchina_pre %>%
                  select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,year,satiation.level,thermal_load,pcGDP_thous90USD,price,`satiation-impedance`) %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      left_join_error_no_match(L101.Pop_thous_province_Yh_gr, by=c("year","region"="province","gcam.consumer")) %>%
      rename(pop_thous_gr = pop_thous) %>%
      left_join_error_no_match(L101.Pop_thous_province, by=c("year","region"="province")) %>%
      rename(pop_thous = pop) %>%
      left_join_error_no_match(L244.SubregionalShares_gcamchina %>%
                                 select(-inc.year.fillout) %>%
                                 rename(year = pop.year.fillout)
                               , by=c("region","gcam.consumer","year")) %>%
      mutate(pcGDP_thous90USD_gr=(pcGDP_thous90USD * 1E3 * pop_thous * 1E3 * subregional.income.share) / (pop_thous_gr * 1E3 * 1E3)) %>%
      mutate(afford = (pcGDP_thous90USD_gr * 1000/def9075) / price) %>%
      mutate(afford = if_else(is.infinite(afford),0,afford)) %>%
      left_join_error_no_match(bind_rows(L244.Floorspace_resid,L244.Floorspace_comm),
                               by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "year")) %>%
      mutate(serv=(satiation.level * (1-exp((-log(2)/`satiation-impedance`)*afford))) * base.building.size) %>%
      # # Adjust coal and TradBio
      # mutate(serv = if_else(grepl("coal",thermal.building.service.input),coef(fit_coal)[1]/(afford),serv),
      #        serv = if_else(grepl("TradBio",thermal.building.service.input),coef(fit_tradBio)[1]/(afford + coef(fit_tradBio)[2]),serv)) %>%
      mutate(serv = if_else(afford == 0, 0, serv))

    # Calculate subtotals (for shares)
    L244.ThermalShares_pre_subt <- L244.ThermalShares_gcamchina_pre %>%
      group_by(region,year,thermal.building.service.input) %>%
      summarise(serv = sum(serv)) %>%
      ungroup() %>%
      rename(serv_aggReg = serv)

    # Merge the subtotals to the estimated values to calculate %shares for each consumer group, in each region and period
    L244.ThermalShares_gcamchina <- L244.ThermalShares_gcamchina_pre %>%
      left_join(L244.ThermalShares_pre_subt
                , by=c("region","thermal.building.service.input","year")) %>%
      mutate(thermal_share = serv / serv_aggReg) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,year,thermal_share) %>%
      replace_na(list(thermal_share = 0)) %>%
      # Check
      group_by(region,nodeInput,building.node.input,thermal.building.service.input,year) %>%
      mutate(agg_thermal_share = sum(thermal_share)) %>%
      ungroup()


    L244.ThermalBaseService_gcamchina <- L244.ThermalBaseService_gcamchina_pre %>%
      select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,year, base.service) %>%
      filter(grepl("resid",nodeInput)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.ThermalBaseService_gcamchina_pre %>%
                  select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,year ,base.service) %>%
                  filter(grepl("comm",nodeInput))) %>%
      left_join_error_no_match(L244.ThermalShares_gcamchina %>%
                                 select(-agg_thermal_share)
                               , by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input","year")) %>%
      mutate(base.service = base.service * thermal_share) %>%
      select(-thermal_share) %>%
      mutate(base.service = round(base.service, energy.DIGITS_SERVICE))


    #------------------------------------------------------
    # Dec 2024, Rongqi
    # Due to the lackness of modern service and traditional service (coal and TradBio), drop this part
    # NOTE: Using the adjusted consumer-level data, parameters for traditional fuel function can be re-fitted to improve the fit.
    # They can also be estimated by service to capture additional differences/dynamics

    # # 1- Re-fit parameters: Coal
    #
    # # YZ- Generic services in GCAM-core are represented as "others" and it has coal as fuel
    # # but in GCAM-China, there's no coal in Generic services (i.e., non-thermal services)
    #
    # # Generic services
    #
    # # Thermal services
    # serv_coal_refit_thermal <- L244.ThermalBaseService_gcamchina %>%
    #   filter(grepl("coal",thermal.building.service.input)) %>%
    #   left_join_error_no_match(L101.pcgdp_thous90USD_province_Yh_gr,
    #                            by = c("year","gcam.consumer","region"="province")) %>%
    #   left_join(L144.prices_bld_gcamchina %>% rename(thermal.building.service.input = sector,price=value),
    #             by = c("region","year","thermal.building.service.input")) %>%
    #   replace_na(list(price = 1)) %>%
    #   mutate(afford = (pcGDP_thous90USD*1000/def9075) / price) %>%
    #   filter(complete.cases(.)) %>%
    #   filter(base.service != 0)
    #
    # formula.coal_refit_thermal <- "base.service~A/(afford+k)"
    # start.value.coal_refit_thermal <- c(A = 1, k = 0.5)
    #
    # fit_coal_refit_thermal <- nls(formula.coal_refit_thermal, serv_coal_refit_thermal, start.value.coal_refit_thermal)
    # A_coal_refit_thermal <- coef(fit_coal_refit_thermal)[1]
    # k_coal_refit_thermal <- coef(fit_coal_refit_thermal)[2]
    #
    # L244.coal.coef <- tibble(gcam.consumer = "resid_rural", nodeInput = "resid_rural", building.node.input = "resid_rural_building") %>%
    #   repeat_add_columns(tibble(service = "resid_rural heating coal")) %>%
    #   bind_rows(tibble(gcam.consumer = "resid_urban", nodeInput = "resid_urban", building.node.input = "resid_urban_building") %>%
    #               repeat_add_columns(tibble(service = "resid_urban heating coal"))) %>%
    #   mutate(A_coal = A_coal_refit_thermal,
    #          k_coal = k_coal_refit_thermal) %>%
    #   repeat_add_columns(tibble(region = unique(serv_coal$region)))
    #
    # # Some regions have coal services at early historical years, but have phased it out in recent historical years (2005-2015)
    # # In these regions we assume there should not be any coal demand in future model periods
    # # We correct this dynamic by zeroing the coefficient in the demand function:
    # check_coal <- L244.base_service %>%
    #   filter(year == MODEL_FINAL_BASE_YEAR, grepl("coal",building.service.input)) %>%
    #   filter(base.service == 0) %>%
    #   mutate(is.no.coal = 1) %>%
    #   select(-base.service)
    #
    # # check_coal_oth<-check_coal %>% filter(grepl("others",building.service.input))
    # check_coal_thermal <- check_coal %>% filter(grepl("heating",building.service.input))
    #
    # # reg_no_recentCoal_oth<-unique(check_coal_oth$region)
    # reg_no_recentCoal_thermal <- unique(check_coal_thermal$region)
    #
    #
    # L244.coal.coef <- L244.coal.coef %>%
    #   mutate(#A_coal = if_else(grepl("others",service) & region %in% reg_no_recentCoal_oth,0,A_coal), #YZ- coal only used in heating in USA
    #     A_coal = if_else(grepl("heating",service) & region %in% reg_no_recentCoal_thermal,0,A_coal))
    #
    #
    # L244.ThermalCoalCoef_gcamchina <- L244.coal.coef %>%
    #   filter(service %in% thermal_services) %>%
    #   rename(thermal.building.service.input = service) %>%
    #   repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
    #   mutate(gcam.consumer = paste0(gcam.consumer,"_",group)) %>%
    #   mutate(year = MODEL_FINAL_BASE_YEAR) %>%
    #   left_join_error_no_match(L244.ThermalBaseService_gcamchina,
    #                            by = c("gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input", "region","year")) %>%
    #   mutate(thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
    #   rename(base.coal = base.service) %>%
    #   mutate(A_coal = round(A_coal, energy.DIGITS_COEFFICIENT),
    #          k_coal = round(k_coal, energy.DIGITS_COEFFICIENT),
    #          base.coal = round(base.coal, energy.DIGITS_SERVICE)) %>%
    #   select(LEVEL2_DATA_NAMES[["ThermalCoalCoef"]])
    #
    # # 2- Re-fit parameters: Traditional Biomass
    #
    # # Generic services
    # # YZ - similar to coal, generic services in China does not have TradBio involved
    # # TradBio is reported only for heating services
    #
    # # Thermal services
    # serv_TradBio_refit_thermal <- L244.ThermalBaseService_gcamchina %>%
    #   filter(grepl("TradBio",thermal.building.service.input)) %>%
    #   left_join_error_no_match(L101.pcgdp_thous90USD_province_Yh_gr,
    #                            by = c("year","gcam.consumer","region"="province")) %>%
    #   left_join(L144.prices_bld_gcamchina %>% rename(thermal.building.service.input = sector, price=value),
    #             by = c("region", "year","thermal.building.service.input")) %>%
    #   replace_na(list(price = 1)) %>%
    #   mutate(afford = (pcGDP_thous90USD*1000/def9075) / price) %>%
    #   filter(complete.cases(.)) %>%
    #   filter(base.service != 0)
    #
    # formula.TradBio_refit_thermal <- "base.service~x/(afford+y)"
    # start.value.TradBio_refit_thermal <- c(x = 10,y = 10)
    #
    # fit_TradBio_refit_thermal <- nls(formula.TradBio_refit_thermal, serv_TradBio_refit_thermal, start.value.TradBio_refit_thermal)
    # x_TradBio_refit_thermal <- coef(fit_TradBio_refit_thermal)[1]
    # y_TradBio_refit_thermal <- coef(fit_TradBio_refit_thermal)[2]
    #
    #
    # L244.tradBio.coef <- tibble(gcam.consumer = "resid_rural", nodeInput = "resid_rural", building.node.input = "resid_rural_building") %>%
    #   repeat_add_columns(tibble(service = "resid_rural heating TradBio")) %>%
    #   bind_rows(tibble(gcam.consumer = "resid_urban", nodeInput = "resid_urban", building.node.input = "resid_urban_building") %>%
    #               repeat_add_columns(tibble(service = "resid_urban heating TradBio"))) %>%
    #   mutate(x_TradBio = x_TradBio_refit_thermal,
    #          y_TradBio = y_TradBio_refit_thermal) %>%
    #   repeat_add_columns(tibble(region = unique(serv_TradBio$region)))
    #
    #
    # L244.ThermalTradBioCoef_gcamchina <- L244.tradBio.coef %>%
    #   filter(service %in% thermal_services) %>%
    #   rename(thermal.building.service.input = service) %>%
    #   repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
    #   mutate(gcam.consumer = paste0(gcam.consumer,"_",group)) %>%
    #   mutate(year =MODEL_FINAL_BASE_YEAR) %>%
    #   left_join_error_no_match(L244.ThermalBaseService_gcamchina,
    #                            by = c("gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input", "region","year")) %>%
    #   mutate(thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
    #   rename(base.TradBio = base.service) %>%
    #   mutate(x_TradBio = if_else(base.TradBio==0,0,x_TradBio)) %>%
    #   mutate(x_TradBio = round(x_TradBio, energy.DIGITS_COEFFICIENT),
    #          y_TradBio = round(y_TradBio, energy.DIGITS_COEFFICIENT),
    #          base.TradBio = round(base.TradBio, energy.DIGITS_SERVICE)) %>%
    #   select(LEVEL2_DATA_NAMES[["ThermalTradBioCoef"]])



    #------------------------------------------------------
    # Bias adder: The difference between the observed and estimated data in final calibration year to capture the "unobservable" effects not captured by the variables in the model equations
    # Calculated using the shares
    # First estimate consumer-specific bias adder for different consumers within each region
    # To ensure that all the consumers are in the same path, make those consumer-specific adders transition to a common adder in 2030
    # This adder is calculated using regional data, and equally spliting the regional adder across consumer-groups.
    # This transition in three periods avoids drastic jumps from final calibration year to first model period.

    # 1- Generic services

    # Coal and TradBio
    L244.GenericServiceAdder_aggObs <- L244.base_service_EJ_serv %>%
      filter(supplysector %in% generic_services) %>%
      rename(building.service.input = supplysector,
             obs = base.service) %>%
      filter(year == MODEL_FINAL_BASE_YEAR)

    # need to address former lines
    L244.GenericServiceAdder_aggObs_gr <- L244.GenericBaseService_gcamchina %>%
      rename(obs = base.service) %>%
      filter(year == MODEL_FINAL_BASE_YEAR)

    # trad_fuels_oth<-c("resid others coal","resid others TradBio")
    # modern_fuels_oth<-c("resid others modern")
    Adder.Conv.Year <- 2030
    ADJ_MODEL_YEARS <- c(MODEL_BASE_YEARS,MODEL_FUTURE_YEARS[MODEL_FUTURE_YEARS >= Adder.Conv.Year])

    # L244.GenericServiceAdder_coal_tradbio_pre<-L244.GenericShares_pre %>%
    #   filter(grepl("resid",building.service.input)) %>%
    #   filter(building.service.input %in% trad_fuels_oth) %>%
    #   select(region,gcam.consumer,nodeInput,building.node.input,building.service.input,year,afford) %>%
    #   separate(gcam.consumer,c("gcam.consumer","group"),sep="_",remove = F) %>%
    #   mutate(gcam.consumer = paste0(gcam.consumer,"_",group),
    #          building.service.input = paste0(building.service.input,"_",group)) %>%
    #   left_join(L244.GenericCoalCoef,by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "building.service.input")) %>%
    #   left_join(L244.GenericTradBioCoef,by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "building.service.input")) %>%
    #   select(-group,-base.TradBio,-base.coal) %>%
    #   rename(A_coal_new = A_coal,
    #          k_coal_new = k_coal,
    #          x_TradBio_new = x_TradBio,
    #          y_TradBio_new = y_TradBio) %>%
    #   mutate(serv = if_else(grepl("coal",building.service.input),A_coal_new / (afford+k_coal_new),x_TradBio_new / (afford+y_TradBio_new))) %>%
    #   select(region,gcam.consumer,nodeInput,building.node.input,building.service.input,year,serv) %>%
    #   mutate(est = serv) %>%
    #   separate(building.service.input,c("building.service.input","group"),sep = "_") %>%
    #   select(region,year,gcam.consumer,building.service.input,est)

    # L244.GenericServiceAdder_coal_tradbio_pre_agg<-L244.GenericServiceAdder_coal_tradbio_pre %>%
    #   group_by(region,year,building.service.input) %>%
    #   mutate(est_agg = sum(est)) %>%
    #   ungroup() %>%
    #   filter(year == MODEL_FINAL_BASE_YEAR) %>%
    #   left_join(L244.GenericServiceAdder_aggObs, by = c("region", "year", "building.service.input")) %>%
    #   filter(complete.cases(.)) %>%
    #   mutate(bias.adder.equal = (obs - est_agg)/n_groups) %>%
    #   mutate(bias.adder = round(bias.adder.equal,9)) %>%
    #   mutate(nodeInput = "resid",
    #          building.node.input = "resid_building") %>%
    #   ungroup() %>%
    #   # adjust zero adder if observed is 0
    #   mutate(bias.adder = if_else(obs==0,0,bias.adder)) %>%
    #   select(region,gcam.consumer,nodeInput,building.node.input,building.service.input,bias.adder)

    # L244.GenericServiceAdder_coal_tradbio<-L244.GenericServiceAdder_coal_tradbio_pre %>%
    #   filter(year == MODEL_FINAL_BASE_YEAR) %>%
    #   left_join_error_no_match(L244.GenericServiceAdder_aggObs_gr, by = c("region", "year", "gcam.consumer", "building.service.input")) %>%
    #   mutate(bias.adder.share = base.service - est,
    #          bias.adder.share = if_else(base.service==0,0,bias.adder.share)) %>%
    #   left_join_error_no_match(L244.GenericServiceAdder_coal_tradbio_pre_agg, by = c("region", "gcam.consumer", "building.service.input", "nodeInput", "building.node.input")) %>%
    #   mutate(bias.adder = if_else(base.service==0,0,bias.adder)) %>%
    #   select(region,gcam.consumer,nodeInput,building.node.input,building.service.input,bias.adder.share,bias.adder.eq=bias.adder) %>%
    #   repeat_add_columns(tibble(year=ADJ_MODEL_YEARS)) %>%
    #   mutate(bias.adder = if_else(year!= MODEL_FINAL_BASE_YEAR,bias.adder.eq,bias.adder.share)) %>%
    #   complete(nesting(region,gcam.consumer, nodeInput,building.node.input,building.service.input), year = c(year, MODEL_YEARS)) %>%
    #   group_by(region,gcam.consumer, nodeInput,building.node.input,building.service.input) %>%
    #   mutate(bias.adder = approx_fun(year, bias.adder, rule = 1)) %>%
    #   ungroup() %>%
    #   select(LEVEL2_DATA_NAMES[["GenericServiceAdder"]])


    # Modern
    L244.GenericServiceAdder_modern_pre <- L244.GenericShares_gcamchina_pre %>%
      # filter(building.service.input %in% modern_fuels_oth) %>% # YZ- all fuel in generic service in China is modern
      filter(grepl('resid', building.service.input)) %>%
      mutate(est = serv) %>%
      select(region,year,gcam.consumer,building.service.input,est)

    L244.GenericServiceAdder_modern_pre_agg <- L244.GenericServiceAdder_modern_pre %>%
      filter(grepl('resid_rural', building.service.input)) %>%
      mutate(nodeInput = "resid_rural",
             building.node.input = "resid_rural_building") %>%
      bind_rows(L244.GenericServiceAdder_modern_pre %>%
                  filter(grepl('resid_urban', building.service.input)) %>%
                  mutate(nodeInput = "resid_urban",
                         building.node.input = "resid_urban_building")) %>%
      group_by(region,year,building.service.input) %>%
      mutate(est_agg = sum(est)) %>%
      ungroup() %>%
      filter(year== MODEL_FINAL_BASE_YEAR) %>%
      left_join(L244.GenericServiceAdder_aggObs, by = c("region", "year", "building.service.input")) %>%
      filter(complete.cases(.)) %>%
      mutate(adder_bm2 = obs - est_agg) %>%
      left_join_error_no_match(L144.flsp_bm2_province_bld, by = c("region"="province", "nodeInput"="sector", "year")) %>%
      mutate(bias.adder = adder_bm2/value) %>%
      # adjust zero adder if observed is 0
      mutate(bias.adder = if_else(obs==0,0,bias.adder)) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,building.service.input,bias.adder)

    L244.GenericServiceAdder_modern <- L244.GenericServiceAdder_modern_pre %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L244.GenericServiceAdder_aggObs_gr, by = c("region", "year", "gcam.consumer", "building.service.input")) %>%
      left_join_error_no_match(L244.Floorspace_gcamchina, by = c("region", "year", "gcam.consumer", "nodeInput", "building.node.input")) %>%
      mutate(bias.adder.share = (obs - est)/base.building.size,
             bias.adder.share = if_else(obs==0,0,bias.adder.share)) %>%
      left_join_error_no_match(L244.GenericServiceAdder_modern_pre_agg,
                               by = c("region", "gcam.consumer", "building.service.input", "nodeInput", "building.node.input")) %>%
      mutate(bias.adder = if_else(obs==0,0,bias.adder)) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,building.service.input,bias.adder.share,bias.adder.eq=bias.adder) %>%
      repeat_add_columns(tibble(year=ADJ_MODEL_YEARS)) %>%
      mutate(bias.adder = if_else(year!= MODEL_FINAL_BASE_YEAR,bias.adder.eq,bias.adder.share)) %>%
      complete(nesting(region,gcam.consumer, nodeInput,building.node.input,building.service.input), year = c(year, MODEL_YEARS)) %>%
      group_by(region,gcam.consumer, nodeInput,building.node.input,building.service.input) %>%
      mutate(bias.adder = approx_fun(year, bias.adder, rule = 1)) %>%
      ungroup() %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceAdder"]])

    # L244.GenericServiceAdder<-bind_rows(L244.GenericServiceAdder_coal_tradbio,L244.GenericServiceAdder_modern) %>%
    L244.GenericServiceAdder_gcamchina <- L244.GenericServiceAdder_modern %>%
      # add commercial
      bind_rows(L244.GenericShares_gcamchina_pre %>%
                  filter(year== MODEL_FINAL_BASE_YEAR) %>%
                  filter(grepl("comm",gcam.consumer)) %>%
                  mutate(bias.adder = 0) %>%
                  select(LEVEL2_DATA_NAMES[["GenericServiceAdder"]])) %>%
      mutate(bias.adder = round(bias.adder,energy.DIGITS_BIAS_ADDER))



    # # 1.5- Generic services per SSP
    # L244.GenericServiceAdder_SSPs<-L244.GenericServiceAdder %>%
    #   repeat_add_columns(tibble(SSP=unique(L244.GenericServiceImpedance_allvars_SSPs$SSP))) %>%
    #   # Split by SSP, creating a list with a tibble for each SSP, then add attributes
    #   split(.$SSP) %>%
    #   lapply(function(df) {
    #     select(df, -SSP) %>%
    #       add_units("Unitless") %>%
    #       add_comments("Bias-correction parameter for non-thermal services") %>%
    #       add_precursors("energy/A44.satiation_flsp_SSPs", "energy/A44.gcam_consumer", "common/GCAM_region_names",
    #                      "energy/A_regions", "L102.pcgdp_thous90USD_Scen_R_Y", "L101.Pop_thous_R_Yh",
    #                      "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh")
    #   })
    #
    # # Assign each tibble in list
    # for(i in names(L244.GenericServiceAdder_SSPs)) {
    #   assign(paste0("L244.GenericServiceAdder_", i), L244.GenericServiceAdder_SSPs[[i]] %>%
    #            add_title(paste0("Satiation Adder: ", i)) %>%
    #            add_legacy_name(paste0("L244.GenericServiceAdder_SSPs_", i)))
    # }


    # 2- Thermal services

    # trad_fuels_thermal <- c("resid_rural heating coal","resid_rural heating TradBio", "resid_urban heating coal", "resid_urban heating coal")
    modern_fuels_thermal <- c("resid_rural cooling modern","resid_rural heating modern", "resid_urban cooling modern","resid_urban heating modern")

    # Coal and TradBio
    # Dec 2024, Rongqi
    # Due to the lackness of modern service and traditional service (coal and TradBio), drop this part

    L244.ThermalServiceAdder_aggObs <- L244.base_service_EJ_serv %>%
      filter(supplysector %in% thermal_services) %>%
      rename(thermal.building.service.input = supplysector,
             obs = base.service) %>%
      filter(year== MODEL_FINAL_BASE_YEAR)

    L244.ThermalServiceAdder_aggObs_gr<-L244.ThermalBaseService_gcamchina %>%
      rename(obs = base.service) %>%
      filter(year== MODEL_FINAL_BASE_YEAR)
    #
    #
    # L244.ThermalServiceAdder_coal_tradbio_pre <- L244.ThermalShares_gcamchina_pre %>%
    #   filter(grepl("resid",thermal.building.service.input)) %>%
    #   filter(thermal.building.service.input %in% trad_fuels_thermal) %>%
    #   select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,year,afford) %>%
    #   separate(gcam.consumer,c("gcam.consumer","group"),sep="_(?=[^_]*$)",remove = F) %>%
    #   mutate(gcam.consumer = paste0(gcam.consumer,"_",group),
    #          thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
    #   left_join(L244.ThermalCoalCoef_gcamchina,by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input")) %>%
    #   left_join(L244.ThermalTradBioCoef_gcamchina,by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "thermal.building.service.input")) %>%
    #   select(-group,-base.TradBio,-base.coal) %>%
    #   rename(A_coal_new = A_coal,
    #          k_coal_new = k_coal,
    #          x_TradBio_new = x_TradBio,
    #          y_TradBio_new = y_TradBio) %>%
    #   mutate(serv = if_else(grepl("coal",thermal.building.service.input),A_coal_new/(afford+k_coal_new),x_TradBio_new/(afford+y_TradBio_new))) %>%
    #   replace_na(list(serv = 0)) %>%
    #   select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,year,serv) %>%
    #   mutate(est = serv) %>%
    #   separate(thermal.building.service.input,c("thermal.building.service.input","group"),sep = "_(?=[^_]*$)") %>%
    #   select(region,year,gcam.consumer,thermal.building.service.input,est)
    #
    # L244.ThermalServiceAdder_coal_tradbio_pre_agg <- L244.ThermalServiceAdder_coal_tradbio_pre %>%
    #   filter(grepl('resid_rural', thermal.building.service.input)) %>%
    #   mutate(nodeInput = "resid_rural",
    #          building.node.input = "resid_rural_building") %>%
    #   bind_rows(L244.ThermalServiceAdder_coal_tradbio_pre %>%
    #               filter(grepl('resid_urban', thermal.building.service.input)) %>%
    #               mutate(nodeInput = "resid_urban",
    #                      building.node.input = "resid_urban_building")) %>%
    #   group_by(region,year,thermal.building.service.input) %>%
    #   mutate(est_agg = sum(est)) %>%
    #   ungroup() %>%
    #   filter(year == MODEL_FINAL_BASE_YEAR) %>%
    #   left_join_error_no_match(L244.ThermalServiceAdder_aggObs, by = c("region", "year", "thermal.building.service.input")) %>%
    #   filter(complete.cases(.)) %>%
    #   mutate(bias.adder.equal = (obs - est_agg)/n_groups) %>%
    #   mutate(bias.adder = round(bias.adder.equal,9)) %>%
    #   ungroup() %>%
    #   # adjust zero adder if observed is 0
    #   mutate(bias.adder = if_else(obs==0,0,bias.adder)) %>%
    #   select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,bias.adder)
    #
    # L244.ThermalServiceAdder_coal_tradbio <- L244.ThermalServiceAdder_coal_tradbio_pre %>%
    #   filter(year== MODEL_FINAL_BASE_YEAR) %>%
    #   left_join_error_no_match(L244.ThermalServiceAdder_aggObs_gr, by = c("region", "year", "gcam.consumer", "thermal.building.service.input")) %>%
    #   mutate(bias.adder.share = obs - est,
    #          bias.adder.share = if_else(obs==0,0,bias.adder.share)) %>%
    #   left_join_error_no_match(L244.ThermalServiceAdder_coal_tradbio_pre_agg, by = c("region", "gcam.consumer", "thermal.building.service.input", "nodeInput", "building.node.input")) %>%
    #   mutate(bias.adder = if_else(obs==0,0,bias.adder)) %>%
    #   select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,bias.adder.share,bias.adder.eq=bias.adder) %>%
    #   repeat_add_columns(tibble(year=ADJ_MODEL_YEARS)) %>%
    #   mutate(bias.adder = if_else(year!= MODEL_FINAL_BASE_YEAR,bias.adder.eq,bias.adder.share)) %>%
    #   complete(nesting(region,gcam.consumer, nodeInput,building.node.input,thermal.building.service.input), year = c(year, MODEL_YEARS)) %>%
    #   group_by(region,gcam.consumer, nodeInput,building.node.input,thermal.building.service.input) %>%
    #   mutate(bias.adder = approx_fun(year, bias.adder, rule = 1)) %>%
    #   ungroup() %>%
    #   select(LEVEL2_DATA_NAMES[["ThermalServiceAdder"]])


    # Modern
    L244.ThermalServiceAdder_modern_pre <- L244.ThermalShares_gcamchina_pre %>%
      filter(thermal.building.service.input %in% modern_fuels_thermal) %>%
      mutate(est = serv) %>%
      select(region,year,gcam.consumer,thermal.building.service.input,est)

    L244.ThermalServiceAdder_modern_pre_agg <- L244.ThermalServiceAdder_modern_pre %>%
      filter(grepl('resid_rural', thermal.building.service.input)) %>%
      mutate(nodeInput = "resid_rural",
             building.node.input = "resid_rural_building") %>%
      bind_rows(L244.ThermalServiceAdder_modern_pre %>%
                  filter(grepl('resid_urban', thermal.building.service.input)) %>%
                  mutate(nodeInput = "resid_urban",
                         building.node.input = "resid_urban_building")) %>%
      group_by(region,year,thermal.building.service.input) %>%
      mutate(est_agg = sum(est)) %>%
      ungroup() %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      left_join(L244.ThermalServiceAdder_aggObs, by = c("region", "year", "thermal.building.service.input")) %>%
      filter(complete.cases(.)) %>%
      mutate(adder_bm2 = obs - est_agg) %>%
      left_join_error_no_match(L144.flsp_bm2_province_bld, by = c("region"="province", "nodeInput"="sector", "year")) %>%
      mutate(bias.adder = adder_bm2/value) %>%
      # adjust zero adder if observed is 0
      mutate(bias.adder = if_else(obs==0,0,bias.adder)) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,bias.adder)

    L244.ThermalServiceAdder_modern <- L244.ThermalServiceAdder_modern_pre %>%
      filter(year== MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L244.ThermalServiceAdder_aggObs_gr, by = c("region", "year", "gcam.consumer", "thermal.building.service.input")) %>%
      left_join_error_no_match(L244.Floorspace_gcamchina, by = c("region", "year", "gcam.consumer", "nodeInput", "building.node.input")) %>%
      mutate(bias.adder.share = (obs - est)/base.building.size,
             bias.adder.share = if_else(obs==0,0,bias.adder.share)) %>%
      filter(complete.cases(.)) %>%
      left_join_error_no_match(L244.ThermalServiceAdder_modern_pre_agg, by = c("region", "gcam.consumer", "thermal.building.service.input", "nodeInput", "building.node.input")) %>%
      mutate(bias.adder = if_else(obs==0,0,bias.adder)) %>%
      select(region,gcam.consumer,nodeInput,building.node.input,thermal.building.service.input,bias.adder.share,bias.adder.eq=bias.adder) %>%
      repeat_add_columns(tibble(year=ADJ_MODEL_YEARS)) %>%
      mutate(bias.adder = if_else(year!= MODEL_FINAL_BASE_YEAR,bias.adder.eq,bias.adder.share)) %>%
      complete(nesting(region,gcam.consumer, nodeInput,building.node.input,thermal.building.service.input), year = c(year, MODEL_YEARS)) %>%
      group_by(region,gcam.consumer, nodeInput,building.node.input,thermal.building.service.input) %>%
      mutate(bias.adder = approx_fun(year, bias.adder, rule = 1)) %>%
      ungroup() %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceAdder"]])

    L244.ThermalServiceAdder_gcamchina <- L244.ThermalServiceAdder_modern %>%
      # add commercial
      bind_rows(L244.ThermalShares_gcamchina_pre %>%
                  filter(year== MODEL_FINAL_BASE_YEAR) %>%
                  filter(grepl("comm",gcam.consumer)) %>%
                  mutate(bias.adder = 0) %>%
                  select(LEVEL2_DATA_NAMES[["ThermalServiceAdder"]])) %>%
      mutate(bias.adder = round(bias.adder,energy.DIGITS_BIAS_ADDER))

    #------------------------------------------------------
    # Once the calibration process is finished at region level, several files need to be adjusted to multiple consumer groups:

    # Satiation level
    L244.GenericServiceSatiation_gcamchina <- L244.GenericServiceSatiation_gcamchina %>%
      filter(grepl("resid",gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.GenericServiceSatiation_gcamchina %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]])

    # # Need to match in the floorspace into the base service table, divide to calculate the service demand
    # # per unit floorspace in the final calibration year. This (increased slightly) is then the minimum satiation level that needs to be read in.
    # L244.GenericServiceSatiation_SSPs <- L244.GenericServiceSatiation_SSPs %>%
    #   filter(grepl("resid",gcam.consumer)) %>%
    #   repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
    #   unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
    #   bind_rows(L244.GenericServiceSatiation_SSPs %>%
    #               filter(grepl("comm",gcam.consumer))) %>%
    #   select(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]],SSP) %>%
    #
    #   # Split by SSP, creating a list with a tibble for each SSP, then add attributes
    #   split(.$SSP) %>%
    #   lapply(function(df) {
    #     select(df, -SSP) %>%
    #       add_units("EJ/billion m2 floorspace") %>%
    #       add_comments("For USA, calculate satiation level as base year service / base year floorspace times multiplier") %>%
    #       add_comments("USA values written to all regions, then we make sure that no satiation level is below base year service per floorspace") %>%
    #       add_precursors("L144.base_service_EJ_serv", "energy/calibrated_techs_bld_det", "common/GCAM_region_names",
    #                      "L144.flsp_bm2_R_res_Yh", "L144.flsp_bm2_R_comm_Yh", "energy/A44.demand_satiation_mult_SSPs")
    #   })

    # # Assign each tibble in list
    # for(i in names(L244.GenericServiceSatiation_SSPs)) {
    #   assign(paste0("L244.GenericServiceSatiation_", i), L244.GenericServiceSatiation_SSPs[[i]] %>%
    #            add_title(paste0("Satiation levels for non-thermal building services: ", i)) %>%
    #            add_legacy_name(paste0("L244.GenericServiceSatiation_", i)))
    # }


    L244.ThermalServiceSatiation_gcamchina <- L244.ThermalServiceSatiation_gcamchina %>%
      filter(grepl("resid",gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.ThermalServiceSatiation_gcamchina %>%
                  filter(grepl("comm",gcam.consumer))) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceSatiation"]])


    # Internal gains scalar
    L244.Intgains_scalar_gcamchina <- L244.Intgains_scalar_gcamchina %>%
      filter(grepl("resid",gcam.consumer)) %>%
      repeat_add_columns(tibble::tibble(cons.groups)) %>%
      separate(cons.groups,c("sector","cons.groups"),sep="_(?=[^_]*$)",remove = F) %>%
      select(-sector) %>%
      mutate(gcam.consumer = paste0(gcam.consumer,"_",cons.groups),
             thermal.building.service.input = paste0(thermal.building.service.input,"_",cons.groups)) %>%
      select(LEVEL2_DATA_NAMES[["Intgains_scalar"]]) %>%
      bind_rows(L244.Intgains_scalar_gcamchina %>%
                  filter(grepl("comm",gcam.consumer)))


    # Shell Efficiency
    # Adjust future years based on the rule used for expanding USA-based efficiency to the RoW
    # Efficiency in region r = Efficiency in USA * (HDD in region r / HDD in USA)^(-0.05)*(GDP per capita in region r / GDP per capita in USA)^elasticity
    # elasticity is assumed to linearly increase from -0.1 to -0.07 over the century
    # Given that DD are similar across consumer groups within each region, the rule is adapted to be exclusively GDP-driven:
    # Efficiency for consumer i = Regional average efficiency  * (GDP per capita for consumer i / Avergae regional GDP per capita)^elasticity

    # First, create the tibble with price elasticities
    # L244.PrElast.shell<-tibble(shell.year=c(min(MODEL_FUTURE_YEARS),max(MODEL_FUTURE_YEARS)),prelast=c(-0.07,-0.1)) %>%
    #   complete(nesting(shell.year=MODEL_FUTURE_YEARS)) %>%
    #   mutate(prelast = if_else(is.na(prelast), approx_fun(shell.year, prelast, rule = 1), prelast)) %>%
    #   repeat_add_columns(tibble(region=unique(GCAM_region_names$region)))


    # L244.ShellConductance_bld_noadj<-L244.ShellConductance_bld_gcamusa %>%
    #   filter(grepl("resid",gcam.consumer)) %>%
    #   repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
    #   unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
    #   bind_rows(L244.ShellConductance_bld_gcamusa %>%
    #               filter(grepl("comm",gcam.consumer))) %>%
    #               filter(year %in% MODEL_BASE_YEARS)
    #
    # L244.ShellConductance_bld_gcamusa<-L244.ShellConductance_bld_gcamusa %>%
    #   filter(grepl("resid",gcam.consumer)) %>%
    #   repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
    #   unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
    #   bind_rows(L244.ShellConductance_bld_gcamusa %>%
    #               filter(grepl("comm",gcam.consumer))) %>%
    #   anti_join(L244.ShellConductance_bld_noadj,
    #             by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "year", "shell.conductance", "shell.year", "floor.to.surface.ratio")) %>%
    #   left_join_error_no_match(L100.pcGDP_thous90usd_state, by = c("region"="state","year")) %>%
    #   rename(pcGDP_thous90USD = value) %>%
    #   left_join_error_no_match(L244.SubregionalShares_gcamusa %>%
    #                              select(-inc.year.fillout) %>%
    #                              rename(year = pop.year.fillout)
    #                            , by = c("region", "gcam.consumer","year")) %>%
    #   # we use population in 2015 as we don't have projections
    #   left_join_error_no_match(L100.Pop_thous_state %>% filter(year==MODEL_FINAL_BASE_YEAR), by = c("region"="state")) %>%
    #   rename(pop_thous=value) %>%
    #   mutate(gdp=pcGDP_thous90USD*1E3*pop_thous*1E3) %>%
    #   mutate(pcGDP_thous90USD_gr=(gdp*subregional.income.share)/(pop_thous*1E3*subregional.population.share*1E3)) %>%
    #   select(region,gcam.consumer,nodeInput,building.node.input,shell.conductance,shell.year,
    #          floor.to.surface.ratio,pcGDP_thous90USD,pcGDP_thous90USD_gr) %>%
    #   left_join_error_no_match(L244.PrElast.shell,by=c("region","shell.year")) %>%
    #   mutate(adj_shell=(pcGDP_thous90USD_gr/pcGDP_thous90USD)^prelast,
    #          shell.conductance=shell.conductance*adj_shell,
    #          year=shell.year) %>%
    #   select(LEVEL2_DATA_NAMES[["ShellConductance"]]) %>%
    #   bind_rows(L244.ShellConductance_bld_noadj) %>%
    #   arrange(region,year,gcam.consumer) %>%
    #   # To avoid big jumps, we linearly interpolate the shell efficiency from final base year to 2100
    #   filter(year %in% c(MODEL_BASE_YEARS,2100)) %>%
    #   complete(nesting(region,gcam.consumer, nodeInput,building.node.input), year = c(year, MODEL_YEARS)) %>%
    #   mutate(shell.year = year) %>%
    #   group_by(region,gcam.consumer, nodeInput,building.node.input) %>%
    #   mutate(shell.conductance = approx_fun(year, shell.conductance, rule = 1),
    #          floor.to.surface.ratio = approx_fun(year, floor.to.surface.ratio, rule = 2)) %>%
    #   ungroup() %>%
    #   mutate(shell.conductance = round(shell.conductance,energy.DIGITS_SHELL))

    L244.ShellConductance_bld_gcamchina <- L244.ShellConductance_bld_gcamchina %>%
      filter(grepl("resid",gcam.consumer)) %>%
      repeat_add_columns(tibble(group=unique(L144.income_shares$group))) %>%
      unite(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      bind_rows(L244.ShellConductance_bld_gcamchina %>%
                  filter(grepl("comm",gcam.consumer)))

    # Supplysectors are also disaggregated at consumer levels (to allow consumer-specific fuel-technology mixes)
    # First, adjust efficiencies and InterganGain I/O ratios
    L244.GlobalTechEff_bld_gcamchina <- add.cg(L244.GlobalTechEff_bld_pre %>% rename(supplysector=sector.name)) %>%
      rename(sector.name=supplysector)

    L244.GlobalTechIntGainOutputRatio_gcamchina <- add.cg(L244.GlobalTechIntGainOutputRatio_pre %>% rename(supplysector=sector.name)) %>%
      rename(sector.name=supplysector)

    # Finally need to calibrate the different technologies at consumer-group level
    shares_resid <- bind_rows(L244.GenericShares_gcamchina %>% rename(share = gen_share,
                                                                      agg.share = agg_gen_share,
                                                                      adj_sector = building.service.input),
                              L244.ThermalShares_gcamchina %>% rename(adj_sector = thermal.building.service.input,
                                                                      share = thermal_share,
                                                                      agg.share = agg_thermal_share)) %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("tmp","group"),sep = "_(?=[^_]*$)",remove = F) %>%
      select(region,year,group,adj_sector,share)

    L244.StubTechCalInput_bld_gcamchina_comm <- L244.StubTechCalInput_bld_gcamchina_pre %>%
      filter(grepl("comm",supplysector))

    L244.StubTechCalInput_bld_gcamchina_resid <- add.cg(L244.StubTechCalInput_bld_gcamchina_pre) %>%
      filter(grepl("resid",supplysector)) %>%
      separate(supplysector,c("adj_sector","group"),sep = "_(?=[^_]*$)",remove = F) %>%
      left_join_error_no_match(shares_resid, by=c("region","year","group","adj_sector")) %>%
      mutate(share = if_else(is.na(share),0,share)) %>%
      mutate(calibrated.value = calibrated.value * share) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCalInput"]])

    L244.StubTechCalInput_bld_gcamchina <- bind_rows(L244.StubTechCalInput_bld_gcamchina_resid,L244.StubTechCalInput_bld_gcamchina_comm)

    # Add consumer group to building.service.input and to thermal.building.service.input
    L244.ThermalBaseService_gcamchina <- L244.ThermalBaseService_gcamchina %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_(?=[^_]*$)",remove = F) %>%
      mutate(thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalBaseService"]]) %>%
      bind_rows(L244.ThermalBaseService_gcamchina %>%
                  filter(grepl("comm",gcam.consumer)))

    L244.ThermalServiceCoef_gcamchina <- L244.ThermalServiceCoef_gcamchina %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_(?=[^_]*$)",remove = F) %>%
      mutate(thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceCoef"]]) %>%
      bind_rows(L244.ThermalServiceCoef_gcamchina %>%
                  filter(grepl("comm",gcam.consumer)))

    L244.ThermalServiceSatiation_gcamchina <- L244.ThermalServiceSatiation_gcamchina %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_(?=[^_]*$)",remove = F) %>%
      mutate(thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceSatiation"]]) %>%
      bind_rows(L244.ThermalServiceSatiation_gcamchina %>%
                  filter(grepl("comm",gcam.consumer)))

    L244.ThermalServiceImpedance_gcamchina <- L244.ThermalServiceImpedance_gcamchina %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_(?=[^_]*$)",remove = F) %>%
      mutate(thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceImpedance"]]) %>%
      bind_rows(L244.ThermalServiceImpedance_gcamchina %>%
                  filter(grepl("comm",gcam.consumer)))

    L244.ThermalServiceAdder_gcamchina <- L244.ThermalServiceAdder_gcamchina %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_(?=[^_]*$)",remove = F) %>%
      mutate(thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServiceAdder"]]) %>%
      bind_rows(L244.ThermalServiceAdder_gcamchina %>%
                  filter(grepl("comm",gcam.consumer)))

    L244.GenericBaseService_gcamchina <- L244.GenericBaseService_gcamchina %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_(?=[^_]*$)",remove = F) %>%
      mutate(building.service.input = paste0(building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["GenericBaseService"]]) %>%
      bind_rows(L244.GenericBaseService_gcamchina %>%
                  filter(grepl("comm",gcam.consumer)))

    L244.GenericServiceCoef_gcamchina <- L244.GenericServiceCoef_gcamchina %>%
      ungroup() %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_(?=[^_]*$)",remove = F) %>%
      mutate(building.service.input = paste0(building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceCoef"]]) %>%
      bind_rows(L244.GenericServiceCoef_gcamchina %>%
                  filter(grepl("comm",gcam.consumer)))

    L244.GenericServiceSatiation_gcamchina <- L244.GenericServiceSatiation_gcamchina %>%
      ungroup() %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_(?=[^_]*$)",remove = F) %>%
      mutate(building.service.input = paste0(building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceSatiation"]]) %>%
      bind_rows(L244.GenericServiceSatiation_gcamchina %>%
                  filter(grepl("comm",gcam.consumer)))

    L244.GenericServiceImpedance_gcamchina <- L244.GenericServiceImpedance_gcamchina %>%
      ungroup() %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_(?=[^_]*$)",remove = F) %>%
      mutate(building.service.input = paste0(building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceImpedance"]]) %>%
      bind_rows(L244.GenericServiceImpedance_gcamchina %>%
                  filter(grepl("comm",gcam.consumer)))

    L244.GenericServiceAdder_gcamchina <- L244.GenericServiceAdder_gcamchina %>%
      filter(grepl("resid",gcam.consumer)) %>%
      separate(gcam.consumer,c("adj","group"), sep = "_(?=[^_]*$)",remove = F) %>%
      mutate(building.service.input = paste0(building.service.input,"_",group)) %>%
      select(LEVEL2_DATA_NAMES[["GenericServiceAdder"]]) %>%
      bind_rows(L244.GenericServiceAdder_gcamchina %>%
                  filter(grepl("comm",gcam.consumer)))

    # yz END of addind big chunks of lines for multi-consumer implementation ----

    #------------------------------------------------------
    # Write the service prices in final calibration year
    # These will be used in the cpp files to compute the adjustment parameter that will account for the difference between read and calculated service prices
    A44.gcam_consumer_resid_urban <- A44.gcam_consumer_resid %>%
      filter(grepl("resid_urban",gcam.consumer))

    A44.gcam_consumer_resid_rural <- A44.gcam_consumer_resid %>%
      filter(grepl("resid_rural",gcam.consumer))

    L244.GenericServicePrice_gcamchina <- L144.prices_bld_gcamchina %>%
      filter(sector %in% generic_services) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      rename(building.service.input = sector,
             price = value) %>%
      filter(grepl("resid_urban",building.service.input)) %>%
      repeat_add_columns(tibble(gcam.consumer = unique(A44.gcam_consumer_resid_urban$gcam.consumer))) %>%
      left_join_error_no_match(A44.gcam_consumer_resid_urban %>% select(gcam.consumer,nodeInput,building.node.input), by = "gcam.consumer") %>%
      separate(gcam.consumer,c("adj","group"), sep = "_(?=[^_]*$)",remove = F) %>%
      mutate(building.service.input = paste0(building.service.input,"_",group)) %>%
      select(-adj,-group) %>%
      bind_rows(L144.prices_bld_gcamchina %>%
                  filter(sector %in% generic_services) %>%
                  filter(year == MODEL_FINAL_BASE_YEAR) %>%
                  rename(building.service.input = sector,
                         price = value) %>%
                  filter(grepl("resid_rural",building.service.input)) %>%
                  repeat_add_columns(tibble(gcam.consumer = unique(A44.gcam_consumer_resid_rural$gcam.consumer))) %>%
                  left_join_error_no_match(A44.gcam_consumer_resid_rural %>% select(gcam.consumer,nodeInput,building.node.input), by = "gcam.consumer") %>%
                  separate(gcam.consumer,c("adj","group"), sep = "_(?=[^_]*$)",remove = F) %>%
                  mutate(building.service.input = paste0(building.service.input,"_",group)) %>%
                  select(-adj,-group)) %>%
      bind_rows(L144.prices_bld_gcamchina %>%
                  filter(sector %in% generic_services) %>%
                  filter(year == MODEL_FINAL_BASE_YEAR) %>%
                  rename(building.service.input = sector,
                         price = value) %>%
                  filter(grepl("comm",building.service.input)) %>%
                  mutate(gcam.consumer = "comm") %>%
                  left_join_error_no_match(A44.gcam_consumer %>% select(gcam.consumer,nodeInput,building.node.input), by = "gcam.consumer")) %>%
      select(LEVEL2_DATA_NAMES[["GenericServicePrice"]])


    L244.ThermalServicePrice_gcamchina <- L144.prices_bld_gcamchina %>%
      filter(sector %in% thermal_services) %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      rename(thermal.building.service.input = sector,
             price = value) %>%
      filter(grepl("resid_urban",thermal.building.service.input)) %>%
      repeat_add_columns(tibble(gcam.consumer = unique(A44.gcam_consumer_resid_urban$gcam.consumer))) %>%
      left_join_error_no_match(A44.gcam_consumer_resid_urban %>% select(gcam.consumer,nodeInput,building.node.input), by = "gcam.consumer") %>%
      separate(gcam.consumer,c("adj","group"), sep = "_(?=[^_]*$)",remove = F) %>%
      mutate(thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
      select(-adj,-group) %>%
      bind_rows(L144.prices_bld_gcamchina %>%
                  filter(sector %in% thermal_services) %>%
                  filter(year == MODEL_FINAL_BASE_YEAR) %>%
                  rename(thermal.building.service.input = sector,
                         price = value) %>%
                  filter(grepl("resid_rural",thermal.building.service.input)) %>%
                  repeat_add_columns(tibble(gcam.consumer = unique(A44.gcam_consumer_resid_rural$gcam.consumer))) %>%
                  left_join_error_no_match(A44.gcam_consumer_resid_rural %>% select(gcam.consumer,nodeInput,building.node.input), by = "gcam.consumer") %>%
                  separate(gcam.consumer,c("adj","group"), sep = "_(?=[^_]*$)",remove = F) %>%
                  mutate(thermal.building.service.input = paste0(thermal.building.service.input,"_",group)) %>%
                  select(-adj,-group)) %>%
      bind_rows(L144.prices_bld_gcamchina %>%
                  filter(sector %in% thermal_services) %>%
                  filter(year == MODEL_FINAL_BASE_YEAR) %>%
                  rename(thermal.building.service.input = sector,
                         price = value) %>%
                  filter(grepl("comm",thermal.building.service.input)) %>%
                  mutate(gcam.consumer = "comm") %>%
                  left_join_error_no_match(A44.gcam_consumer %>% select(gcam.consumer,nodeInput,building.node.input), by = "gcam.consumer")) %>%
      select(LEVEL2_DATA_NAMES[["ThermalServicePrice"]])

    #------------------------------------------------------
    # Finally, calculate the base year service density
    # This density will be used in case it gets negative when adding the bias adder coefficient
    L244.GenericBaseDens_gcamchina <- L244.GenericBaseService_gcamchina %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L244.Floorspace_gcamchina, by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "year")) %>%
      mutate(base.density = base.service / base.building.size) %>%
      replace_na(list(base.density = 0)) %>%
      select(LEVEL2_DATA_NAMES[["GenericBaseDens"]])

    L244.ThermalBaseDens_gcamchina <- L244.ThermalBaseService_gcamchina %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      left_join_error_no_match(L244.Floorspace_gcamchina, by = c("region", "gcam.consumer", "nodeInput", "building.node.input", "year")) %>%
      mutate(base.density = base.service / base.building.size) %>%
      replace_na(list(base.density = 0)) %>%
      select(LEVEL2_DATA_NAMES[["ThermalBaseDens"]])



    # ===================================================
    # Produce outputs
    L244.DeleteConsumer_CHINAbld %>%
      add_title("Deletes building sector in CHINA region to rewrite with GCAM-CHINA data") %>%
      add_units("NA") %>%
      add_comments("gcam.consumer column from A44.gcam_consumer") %>%
      add_legacy_name("L244.DeleteConsumer_CHINAbld") %>%
      add_precursors("energy/A44.gcam_consumer","L244.Supplysector_bld","socioeconomics/income_shares") ->
      L244.DeleteConsumer_CHINAbld

    L244.DeleteSupplysector_CHINAbld %>%
      add_title("Deletes building sector in CHINA region to rewrite with GCAM-CHINA data") %>%
      add_units("NA") %>%
      add_comments("supplysector column from A44.sector") %>%
      add_legacy_name("L244.DeleteSupplysector_CHINAbld") %>%
      add_precursors("energy/A44.sector","L244.Supplysector_bld","socioeconomics/income_shares") ->
      L244.DeleteSupplysector_CHINAbld

    L244.SubregionalShares_gcamchina %>%
      add_title("Subregional population and income shares") %>%
      add_units("Unitless") %>%
      add_comments("Default values used for years and shares") %>%
      add_legacy_name("L244.SubregionalShares") %>%
      add_precursors("gcam-china/A44.gcam_consumer","gcam-china/province_decile_gdp_per_capita_projections",
                     "gcam-china/urban_pop_share_province", "gcam-china/urban_income_share_province") ->
      L244.SubregionalShares_gcamchina

    L244.PriceExp_IntGains_gcamchina %>%
      add_title("Price exponent on floorspace and naming of internal gains trial markets") %>%
      add_units("Unitless") %>%
      add_comments("A44.gcam_consumer written to all provinces") %>%
      add_legacy_name("L244.PriceExp_IntGains") %>%
      add_precursors("gcam-china/A44.gcam_consumer") ->
      L244.PriceExp_IntGains_gcamchina

    L244.Floorspace_gcamchina %>%
      add_title("base year floorspace") %>%
      add_units("billion m2") %>%
      add_comments("Data from L144.flsp_bm2_province_bld") %>%
      add_legacy_name("L244.Floorspace") %>%
      add_precursors("L144.flsp_bm2_province_bld", "gcam-china/A44.gcam_consumer") ->
      L244.Floorspace_gcamchina

    L244.DemandFunction_serv_gcamchina %>%
      add_title("Service demand function types") %>%
      add_units("NA") %>%
      add_comments("A44.demandFn_serv written to all provinces") %>%
      add_legacy_name("L244.DemandFunction_serv") %>%
      add_precursors("gcam-china/A44.demandFn_serv") ->
      L244.DemandFunction_serv_gcamchina

    L244.DemandFunction_flsp_gcamchina %>%
      add_title("Floorspace demand function types") %>%
      add_units("NA") %>%
      add_comments("A44.demandFn_flsp written to all provinces") %>%
      add_legacy_name("L244.DemandFunction_flsp") %>%
      add_precursors("gcam-china/A44.demandFn_flsp") ->
      L244.DemandFunction_flsp_gcamchina

    L244.Satiation_flsp_gcamchina %>%
      add_title("Satiation levels assumed for floorspace") %>%
      add_units("million m2 / person") %>%
      add_comments("Values from A44.satiation_flsp or L244.Floorspace_gcamchina/L101.Pop_thous_province") %>%
      add_comments("Whichever is larger") %>%
      add_legacy_name("L244.Satiation_flsp") %>%
      add_precursors("gcam-china/A44.satiation_flsp", "gcam-china/A44.gcam_consumer", "L101.Pop_thous_province",
                     "L144.flsp_bm2_province_bld") ->
      L244.Satiation_flsp_gcamchina

    L244.SatiationAdder_gcamchina %>%
      add_title("Satiation adders in floorspace demand function") %>%
      add_units("million m2 / person") %>%
      add_comments("Calculated with function dependent on satiation level; per capita floorspace; and per capita GDP") %>%
      add_legacy_name("L244.SatiationAdder") %>%
      add_precursors("gcam-china/A44.satiation_flsp", "gcam-china/A44.gcam_consumer", "L101.Pop_thous_province",
                     "L144.flsp_bm2_province_bld", "L101.pcGDP_thous90usd_province","gcam-china/urban_pop_share_province") ->
      L244.SatiationAdder_gcamchina

    L244.ThermalBaseService_gcamchina %>%
      add_title("Base year output of thermal buildings services") %>%
      add_units("EJ") %>%
      add_comments("Multiplied energy consumption by efficiency for each technology, then aggregated by service") %>%
      add_legacy_name("L244.ThermalBaseService") %>%
      add_precursors("L144.in_EJ_province_bld_F_U", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.globaltech_eff", "gcam-china/A44.globaltech_eff_avg", "gcam-china/A44.globaltech_shares",
                     "gcam-china/A44.gcam_consumer") ->
      L244.ThermalBaseService_gcamchina

    L244.GenericBaseService_gcamchina %>%
      add_title("Base year output of generic buildings services") %>%
      add_units("EJ") %>%
      add_comments("Multiplied energy consumption by efficiency for each technology, then aggregated by service") %>%
      add_legacy_name("L244.GenericBaseService") %>%
      add_precursors("L144.in_EJ_province_bld_F_U", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.globaltech_eff", "gcam-china/A44.globaltech_eff_avg", "gcam-china/A44.globaltech_shares",
                     "gcam-china/A44.gcam_consumer") ->
      L244.GenericBaseService_gcamchina

    L244.ThermalServiceSatiation_gcamchina %>%
      add_title("Satiation levels assumed for thermal building services") %>%
      add_units("EJ/billion m2 floorspace") %>%
      add_comments("Satiation level = base service / floorspace * exogenous multiplier") %>%
      add_legacy_name("L244.ThermalServiceSatiation") %>%
      add_precursors("L144.in_EJ_province_bld_F_U", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.globaltech_eff", "gcam-china/A44.globaltech_eff_avg", "gcam-china/A44.globaltech_shares",
                     "gcam-china/A44.gcam_consumer", "L144.flsp_bm2_province_bld",
                     "gcam-china/A44.demand_satiation_mult") ->
      L244.ThermalServiceSatiation_gcamchina

    L244.GenericServiceSatiation_gcamchina %>%
      add_title("Satiation levels assumed for non-thermal building services") %>%
      add_units("EJ/billion m2 floorspace") %>%
      add_comments("Satiation level = base service / floorspace * exogenous multiplier") %>%
      add_legacy_name("L244.GenericServiceSatiation") %>%
      add_precursors("L144.in_EJ_province_bld_F_U", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.globaltech_eff", "gcam-china/A44.globaltech_eff_avg", "gcam-china/A44.globaltech_shares",
                     "gcam-china/A44.gcam_consumer", "L144.flsp_bm2_province_bld",
                     "gcam-china/A44.demand_satiation_mult") ->
      L244.GenericServiceSatiation_gcamchina

    L244.Intgains_scalar_gcamchina %>%
      add_title("Scalers relating internal gain energy to increased/reduced cooling/heating demands") %>%
      add_units("Unitless") %>%
      add_comments("internal.gains.scalar = exogenous scalar * degree.days / exogenous degree day norm") %>%
      add_legacy_name("L244.Intgains_scalar") %>%
      add_precursors("L144.in_EJ_province_bld_F_U", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.globaltech_eff", "gcam-china/A44.globaltech_eff_avg", "gcam-china/A44.globaltech_shares",
                     "gcam-china/A44.gcam_consumer", "L144.flsp_bm2_province_bld",
                     "gcam-china/A44.demand_satiation_mult", "L143.HDDCDD_scen_R_Y") ->
      L244.Intgains_scalar_gcamchina

    L244.ShellConductance_bld_gcamchina %>%
      add_title("Shell conductance (inverse of shell efficiency) by province") %>%
      add_units("Unitless") %>%
      add_comments("values from A44.bld_shell_conductance") %>%
      add_legacy_name("L244.ShellConductance_bld") %>%
      add_precursors("gcam-china/A44.bld_shell_conductance", "gcam-china/A44.gcam_consumer") ->
      L244.ShellConductance_bld_gcamchina

    L244.Supplysector_bld_gcamchina %>%
      add_title("Supplysector info for buildings") %>%
      add_units("Unitless") %>%
      add_comments("A44.sector written to all provinces") %>%
      add_legacy_name("L244.Supplysector_bld") %>%
      add_precursors("gcam-china/A44.sector") ->
      L244.Supplysector_bld_gcamchina

    L244.FinalEnergyKeyword_bld_gcamchina %>%
      add_title("Supply sector keywords for detailed building sector") %>%
      add_units("NA") %>%
      add_comments("A44.sector written to all provinces") %>%
      add_legacy_name("L244.FinalEnergyKeyword_bld") %>%
      add_precursors("gcam-china/A44.sector") ->
      L244.FinalEnergyKeyword_bld_gcamchina

    if(exists("L244.SubsectorShrwt_bld_gcamchina")) {
      L244.SubsectorShrwt_bld_gcamchina %>%
        add_title("Subsector shareweights of building sector") %>%
        add_units("Unitless") %>%
        add_comments("A44.subsector_shrwt written to all regions") %>%
        add_legacy_name("L244.SubsectorShrwt_bld") %>%
        add_precursors("gcam-china/A44.subsector_shrwt") ->
        L244.SubsectorShrwt_bld_gcamchina
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorShrwt_bld") ->
        L244.SubsectorShrwt_bld_gcamchina
    }

    if(exists("L244.SubsectorShrwtFllt_bld_gcamchina")) {
      L244.SubsectorShrwtFllt_bld_gcamchina %>%
        add_title("Subsector shareweights of building sector") %>%
        add_units("Unitless") %>%
        add_comments("A44.subsector_shrwt written to all regions") %>%
        add_legacy_name("L244.SubsectorShrwtFllt_bld") %>%
        add_precursors("gcam-china/A44.subsector_shrwt") ->
        L244.SubsectorShrwtFllt_bld_gcamchina
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorShrwtFllt_bld") ->
        L244.SubsectorShrwtFllt_bld_gcamchina
    }

    if(exists("L244.SubsectorInterp_bld_gcamchina")) {
      L244.SubsectorInterp_bld_gcamchina %>%
        add_title("Subsector shareweight interpolation of building sector") %>%
        add_units("NA") %>%
        add_comments("A44.subsector_interp written to all regions") %>%
        add_legacy_name("L244.SubsectorInterp_bld") %>%
        add_precursors("gcam-china/A44.subsector_interp") ->
        L244.SubsectorInterp_bld_gcamchina
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorInterp_bld") ->
        L244.SubsectorInterp_bld_gcamchina
    }

    if(exists("L244.SubsectorInterpTo_bld_gcamchina")) {
      L244.SubsectorInterpTo_bld_gcamchina %>%
        add_title("Subsector shareweight interpolation of building sector") %>%
        add_units("NA") %>%
        add_comments("A44.subsector_interp written to all regions") %>%
        add_legacy_name("L244.SubsectorInterpTo_bld") %>%
        add_precursors("gcam-china/A44.subsector_interp") ->
        L244.SubsectorInterpTo_bld_gcamchina
    } else {
      missing_data() %>%
        add_legacy_name("L244.SubsectorInterpTo_bld") ->
        L244.SubsectorInterpTo_bld_gcamchina
    }

    L244.SubsectorLogit_bld_gcamchina %>%
      add_title("Subsector logit exponents of building sector") %>%
      add_units("Unitless") %>%
      add_comments("A44.subsector_logit written to all provinces") %>%
      add_legacy_name("L244.SubsectorLogit_bld") %>%
      add_precursors("gcam-china/A44.subsector_logit") ->
      L244.SubsectorLogit_bld_gcamchina

    L244.StubTech_bld_gcamchina %>%
      add_title("Identification of stub technologies for buildings") %>%
      add_units("NA") %>%
      add_comments("A44.globaltech_eff written to all provinces") %>%
      add_legacy_name("L244.StubTech_bld") %>%
      add_precursors("gcam-china/A44.globaltech_eff") ->
      L244.StubTech_bld_gcamchina

    L244.StubTechCalInput_bld_gcamchina %>%
      add_title("Calibrated energy consumption and share weights by buildings technologies") %>%
      add_units("calibrated.value: EJ/yr; shareweights: Unitless") %>%
      add_comments("Energy consumption multiplied by shares to get calibrated energy") %>%
      add_comments("Shares calculated using efficiency averages") %>%
      add_legacy_name("L244.StubTechCalInput_bld") %>%
      add_precursors("L144.in_EJ_province_bld_F_U", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.globaltech_eff", "gcam-china/A44.globaltech_eff_avg", "gcam-china/A44.globaltech_shares") ->
      L244.StubTechCalInput_bld_gcamchina

    L244.StubTechMarket_bld_gcamchina %>%
      add_title("market names for fuel inputs to all technologies in each province") %>%
      add_units("NA") %>%
      add_comments("Categories from A44.globaltech_eff written to all provinces") %>%
      add_comments("Market set to provinces for electricity") %>%
      add_legacy_name("L244.StubTechMarket_bld_gcamchina") %>%
      add_precursors("gcam-china/province_names_mappings") ->
      L244.StubTechMarket_bld_gcamchina

    L244.GlobalTechIntGainOutputRatio_gcamchina %>%
      add_title("Output ratios of internal gain energy from non-thermal building services") %>%
      add_units("Unitless") %>%
      add_comments("internal.gains.output.ratio = input.ratio from A44.globaltech_intgains divided by efficiency from L244.GlobalTechEff_bld_gcamchina") %>%
      add_legacy_name("L244.GlobalTechIntGainOutputRatio_gcamchina") %>%
      add_precursors("gcam-china/A44.globaltech_intgains", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.gcam_consumer", "gcam-china/A44.globaltech_eff") ->
      L244.GlobalTechIntGainOutputRatio_gcamchina

    L244.GlobalTechInterpTo_bld_gcamchina %>%
      add_title("Technology shareweight interpolation") %>%
      add_units("NA") %>%
      add_comments("Directly from A44.globaltech_interp") %>%
      add_legacy_name("L244.GlobalTechInterpTo_bld_gcamchina") %>%
      add_precursors("gcam-china/A44.globaltech_interp") ->
      L244.GlobalTechInterpTo_bld_gcamchina

    L244.GlobalTechEff_bld_gcamchina %>%
      add_title("Assumed efficiencies (all years) of buildings technologies") %>%
      add_units("Unitless") %>%
      add_comments("Values from A44.globaltech_eff") %>%
      add_legacy_name("L244.GlobalTechEff_bld_gcamchina") %>%
      add_precursors("gcam-china/A44.globaltech_eff") ->
      L244.GlobalTechEff_bld_gcamchina

    L244.GlobalTechShrwt_bld_gcamchina %>%
      add_title("Default shareweights for global building technologies") %>%
      add_units("Unitless") %>%
      add_comments("Values interpolated to model years from A44.globaltech_shrwt") %>%
      add_legacy_name("L244.GlobalTechShrwt_bld") %>%
      add_precursors("gcam-china/A44.globaltech_shrwt") ->
      L244.GlobalTechShrwt_bld_gcamchina

    L244.GlobalTechCost_bld_gcamchina %>%
      add_title("Non-fuel costs of global building technologies") %>%
      add_units("1975$/GJ") %>%
      add_comments("Values from A44.globaltech_cost") %>%
      add_legacy_name("L244.GlobalTechCost_bld") %>%
      add_precursors("gcam-china/A44.globaltech_cost") ->
      L244.GlobalTechCost_bld_gcamchina

    L244.GlobalTechSCurve_bld_gcamchina %>%
      add_title("Retirement rates for building technologies") %>%
      add_units("lifetime/half.life = years") %>%
      add_comments("Lifetime, steepness, and half.life from A44.globaltech_retirement") %>%
      add_legacy_name("L244.GlobalTechSCurve_bld_gcamchina") %>%
      add_precursors("gcam-china/A44.globaltech_cost", "gcam-china/A44.globaltech_retirement") ->
      L244.GlobalTechSCurve_bld_gcamchina

    L244.HDDCDD_A2_CCSM3x_China %>%
      add_title("Heating and Cooling Degree Days by Province for CCSM3x A2") %>%
      add_units("Fahrenheit Degree Days") %>%
      add_comments("L143.HDDCDD_scen_province assigned to GCAM subsectors") %>%
      add_legacy_name("L244.HDDCDD_A2_CCSM3x") %>%
      add_precursors("L143.HDDCDD_scen_R_Y", "gcam-china/A44.sector",
                     "gcam-china/calibrated_techs_bld_china", "gcam-china/A44.gcam_consumer") ->
      L244.HDDCDD_A2_CCSM3x_China

    L244.HDDCDD_constdds_China %>%
      add_title("Heating and Cooling Degree Days by Province - constant at historical levels") %>%
      add_units("Fahrenheit Degree Days") %>%
      add_comments("L143.HDDCDD_scen_province assigned to GCAM subsectors") %>%
      same_precursors_as("L244.HDDCDD_A2_CCSM3x_China") ->
      L244.HDDCDD_constdds_China

    L244.GompFnParam_gcamchina %>%
      add_title("Parameters for the floorspace Gompertz function") %>%
      add_units("Unitless") %>%
      add_comments("Computed offline based on data from RECS and IEA") %>%
      add_legacy_name("L244.GompFnParam_gcamchina") %>%
      add_precursors("L144.flsp_param", "L101.Pop_thous_province","L101.pcGDP_thous90usd_province",
                     "gcam-china/A44.hab_land_flsp_china","L144.flsp_bm2_province_bld") ->
      L244.GompFnParam_gcamchina

    L244.Satiation_impedance_gcamchina %>%
      add_title("Satiation impedance for floorspace") %>%
      add_units("Unitless") %>%
      add_comments("Calibrated in the DS for flexibility with multiple consumer groups") %>%
      add_legacy_name("L244.Satiation_impedance_gcamchina") %>%
      add_precursors("gcam-china/A44.satiation_flsp", "gcam-china/A44.gcam_consumer", "L101.Pop_thous_province",
                     "L144.flsp_bm2_province_bld") ->
      L244.Satiation_impedance_gcamchina

    L244.GenericServiceImpedance_gcamchina %>%
      add_title("Satiation impedance for non-thermal building services") %>%
      add_units("unitless") %>%
      add_comments("Satiation impedance estimated using final base year information") %>%
      add_legacy_name("L244.GenericServiceImpedance_gcamchina") %>%
      add_precursors("L144.in_EJ_province_bld_F_U", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.globaltech_eff", "gcam-china/A44.globaltech_eff_avg", "gcam-china/A44.globaltech_shares",
                     "gcam-china/A44.gcam_consumer", "L144.flsp_bm2_province_bld", "gcam-china/A44.CalPrice_service_gcamchina") ->
      L244.GenericServiceImpedance_gcamchina

    L244.GenericServiceCoef_gcamchina %>%
      add_title("Calibration coefficient for non-thermal building services") %>%
      add_units("unitless") %>%
      add_comments("Estimated using final base year information") %>%
      add_legacy_name("L244.GenericServiceCoef_gcamchina") %>%
      add_precursors("L144.in_EJ_province_bld_F_U", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.globaltech_eff", "gcam-china/A44.globaltech_eff_avg", "gcam-china/A44.globaltech_shares",
                     "gcam-china/A44.gcam_consumer", "L144.flsp_bm2_province_bld", "gcam-china/A44.CalPrice_service_gcamchina") ->
      L244.GenericServiceCoef_gcamchina

    L244.GenericServiceAdder_gcamchina %>%
      add_title("Bias Adder for non-thermal building services") %>%
      add_units("unitless") %>%
      add_comments("Estimated using final base year information") %>%
      add_legacy_name("L244.GenericServiceAdder_gcamchina") %>%
      add_precursors("L144.in_EJ_province_bld_F_U", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.globaltech_eff", "gcam-china/A44.globaltech_eff_avg", "gcam-china/A44.globaltech_shares",
                     "gcam-china/A44.gcam_consumer", "L144.flsp_bm2_province_bld", "gcam-china/A44.CalPrice_service_gcamchina") ->
      L244.GenericServiceAdder_gcamchina

    L244.ThermalServiceImpedance_gcamchina %>%
      add_title("Satiation impedance for thermal building services") %>%
      add_units("unitless") %>%
      add_comments("Satiation impedance estimated using final base year information") %>%
      add_legacy_name("L244.ThermalServiceImpedance_gcamchina") %>%
      add_precursors("L144.in_EJ_province_bld_F_U", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.globaltech_eff", "gcam-china/A44.globaltech_eff_avg", "gcam-china/A44.globaltech_shares",
                     "gcam-china/A44.gcam_consumer", "L144.flsp_bm2_province_bld", "gcam-china/A44.CalPrice_service_gcamchina") ->
      L244.ThermalServiceImpedance_gcamchina

    L244.ThermalServiceCoef_gcamchina %>%
      add_title("Calibration coefficient for non-thermal building services") %>%
      add_units("unitless") %>%
      add_comments("Estimated using final base year information") %>%
      add_legacy_name("L244.ThermalServiceCoef_gcamchina") %>%
      add_precursors("L144.in_EJ_province_bld_F_U", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.globaltech_eff", "gcam-china/A44.globaltech_eff_avg", "gcam-china/A44.globaltech_shares",
                     "gcam-china/A44.gcam_consumer", "L144.flsp_bm2_province_bld", "gcam-china/A44.CalPrice_service_gcamchina") ->
      L244.ThermalServiceCoef_gcamchina

    L244.ThermalServiceAdder_gcamchina %>%
      add_title("Bias Adder for non-thermal building services") %>%
      add_units("unitless") %>%
      add_comments("Estimated using final base year information") %>%
      add_legacy_name("L244.ThermalServiceAdder_gcamchina") %>%
      add_precursors("L144.in_EJ_province_bld_F_U", "gcam-china/calibrated_techs_bld_china",
                     "gcam-china/A44.globaltech_eff", "gcam-china/A44.globaltech_eff_avg", "gcam-china/A44.globaltech_shares",
                     "gcam-china/A44.gcam_consumer", "L144.flsp_bm2_province_bld", "gcam-china/A44.CalPrice_service_gcamchina") ->
      L244.ThermalServiceAdder_gcamchina

    L244.GenericServicePrice_gcamchina %>%
      add_title("Final-base-year service prices") %>%
      add_units("$1975/GJ") %>%
      add_comments("Prices for generic services") %>%
      add_legacy_name("L244.GenericServicePrice_gcamchina") %>%
      add_precursors("gcam-china/A44.CalPrice_service_gcamchina") ->
      L244.GenericServicePrice_gcamchina

    L244.ThermalServicePrice_gcamchina %>%
      add_title("Final-base-year service prices") %>%
      add_units("$1975/GJ") %>%
      add_comments("Prices for thermal services") %>%
      add_legacy_name("L244.ThermalServicePrice_gcamchina") %>%
      add_precursors("gcam-china/A44.CalPrice_service_gcamchina") ->
      L244.ThermalServicePrice_gcamchina

    L244.GenericBaseDens_gcamchina %>%
      add_title("Final-base-year service density") %>%
      add_units("$1975/GJ") %>%
      add_comments("Service density for generic services") %>%
      add_legacy_name("L244.GenericBaseDens_gcamchina") %>%
      add_precursors("L144.in_EJ_province_bld_F_U","L144.flsp_bm2_province_bld") ->
      L244.GenericBaseDens_gcamchina

    L244.ThermalBaseDens_gcamchina %>%
      add_title("Final-base-year service density") %>%
      add_units("$1975/GJ") %>%
      add_comments("Service density for thermal services") %>%
      add_legacy_name("L244.ThermalBaseDens_gcamchina") %>%
      add_precursors("L144.in_EJ_province_bld_F_U","L144.flsp_bm2_province_bld") ->
      L244.ThermalBaseDens_gcamchina

    if(exists("L244.DeleteGenericService_gcamchina")) {
      L244.DeleteGenericService_gcamchina %>%
        add_title("Removing non-existent services") %>%
        add_units("NA") %>%
        add_comments("Categories from L244.GenericBaseService_gcamchina with no base.service") %>%
        add_legacy_name("L244.DeleteGenericService_gcamchina") %>%
        same_precursors_as(L244.GenericBaseService_gcamchina) ->
        L244.DeleteGenericService_gcamchina
    } else {
      missing_data() %>%
        add_legacy_name("L244.DeleteGenericService_gcamchina") ->
        L244.DeleteGenericService_gcamchina
    }

    L244.DeleteThermalService_gcamchina %>%
      add_title("Removing non-existent thermal services") %>%
      add_units("NA") %>%
      add_comments("Categories from L244.ThermalBaseService_gcamusa with no base.service") %>%
      add_legacy_name("L244.DeleteThermalService_gcamchina") %>%
      same_precursors_as(L244.ThermalBaseService_gcamchina) ->
      L244.DeleteThermalService_gcamchina

    L244.ThermalShares_gcamchina %>%
      add_title("Shares for allocate thermal services across income groups") %>%
      add_units("%") %>%
      add_comments("Calculated using pc_thous") %>%
      add_legacy_name("L244.ThermalShares_gcamchina") %>%
      add_precursors("L144.in_EJ_province_bld_F_U","gcam-china/urban_pop_share_province") ->
      L244.ThermalShares_gcamchina

    L244.GenericShares_gcamchina %>%
      add_title("Shares for allocate generic services across income groups") %>%
      add_units("%") %>%
      add_comments("Calculated using pc_thous") %>%
      add_legacy_name("L244.GenericShares_gcamchina") %>%
      add_precursors("L144.in_EJ_province_bld_F_U","gcam-china/urban_pop_share_province") ->
      L244.GenericShares_gcamchina

    L244.FuelPrefElast_bld_gcamchina %>%
      add_title("Fuel preference elasticities of building energy use") %>%
      add_units("NA") %>%
      add_comments("Elasticity dictating how share weights change with GDP per capita") %>%
      add_legacy_name("L244.FuelPrefElast_bld_gcamchina") %>%
      add_precursors("gcam-china/A44.fuelprefElasticity") ->
      L244.FuelPrefElast_bld_gcamchina


    return_data(L244.DeleteConsumer_CHINAbld,
                L244.DeleteSupplysector_CHINAbld,
                L244.SubregionalShares_gcamchina,
                L244.PriceExp_IntGains_gcamchina,
                L244.Floorspace_gcamchina,
                L244.DemandFunction_serv_gcamchina,
                L244.DemandFunction_flsp_gcamchina,
                L244.Satiation_flsp_gcamchina,
                L244.SatiationAdder_gcamchina,
                L244.ThermalBaseService_gcamchina,
                L244.GenericBaseService_gcamchina,
                L244.ThermalServiceSatiation_gcamchina,
                L244.GenericServiceSatiation_gcamchina,
                L244.Intgains_scalar_gcamchina,
                L244.ShellConductance_bld_gcamchina,
                L244.Supplysector_bld_gcamchina,
                L244.FinalEnergyKeyword_bld_gcamchina,
                L244.SubsectorShrwt_bld_gcamchina,
                L244.SubsectorShrwtFllt_bld_gcamchina,
                L244.SubsectorInterp_bld_gcamchina,
                L244.SubsectorInterpTo_bld_gcamchina,
                L244.SubsectorLogit_bld_gcamchina,
                L244.StubTech_bld_gcamchina,
                L244.StubTechCalInput_bld_gcamchina,
                L244.StubTechMarket_bld_gcamchina,
                L244.GlobalTechIntGainOutputRatio_gcamchina,
                L244.GlobalTechInterpTo_bld_gcamchina,
                L244.GlobalTechEff_bld_gcamchina,
                L244.GlobalTechShrwt_bld_gcamchina,
                L244.GlobalTechCost_bld_gcamchina,
                L244.GlobalTechSCurve_bld_gcamchina,
                L244.HDDCDD_A2_CCSM3x_China,
                L244.HDDCDD_constdds_China,
                L244.GompFnParam_gcamchina,
                L244.Satiation_impedance_gcamchina,
                L244.GenericServiceImpedance_gcamchina,
                L244.GenericServiceCoef_gcamchina,
                L244.GenericServiceAdder_gcamchina,
                L244.ThermalServiceImpedance_gcamchina,
                L244.ThermalServiceCoef_gcamchina,
                L244.ThermalServiceAdder_gcamchina,
                L244.GenericServicePrice_gcamchina,
                L244.ThermalServicePrice_gcamchina,
                L244.GenericBaseDens_gcamchina,
                L244.ThermalBaseDens_gcamchina,
                L244.DeleteThermalService_gcamchina,
                L244.DeleteGenericService_gcamchina,
                L244.GenericShares_gcamchina,
                L244.ThermalShares_gcamchina,
                L244.FuelPrefElast_bld_gcamchina
    )
  } else {
    stop("Unknown command")
  }
}
