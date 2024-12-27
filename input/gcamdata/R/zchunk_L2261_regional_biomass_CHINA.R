# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L2261.regional_biomass_CHINA
#'
#' Create biomass supply sectors at the provincial level.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2261.DeleteSupplysector_bio_CHINA}, \code{L2261.Supplysector_bio_CHINA},
#' \code{L2261.SubsectorShrwtFllt_bio_CHINA}, \code{L2261.SubsectorInterp_bio_CHINA}, \code{L2261.SubsectorLogit_bio_CHINA},
#' \code{L2261.StubTech_bio_CHINA}, \code{L2261.StubTechMarket_bio_CHINA}, \code{L2261.StubTechShrwt_rbO_CHINA},
#' \code{L2261.StubTechFractSecOut_bio_CHINA}, \code{L2261.StubTechFractProd_bio_CHINA}, \code{L2261.Rsrc_DDGS_CHINA},
#' \code{L2261.RsrcPrice_DDGS_CHINA}, \code{L2261.Tech_rbm_CHINA}, \code{L2261.TechShrwt_rbm_CHINA},
#' \code{L2261.TechCoef_rbm_CHINA}, \code{L2261.Tech_dbm_CHINA}, \code{L2261.TechShrwt_dbm_CHINA},
#' \code{L2261.TechEff_dbm_CHINA}, \code{L2261.TechCost_dbm_CHINA}, \code{L2261.CarbonCoef_bio_CHINA},
#' \code{L2261.StubTechMarket_en_CHINA}, \code{L2261.StubTechMarket_elecS_CHINA}, \code{L2261.StubTechMarket_ind_CHINA},
#' \code{L2261.StubTechMarket_cement_CHINA}, \code{L2261.StubTechMarket_bld_CHINA}.
#' The corresponding file in the original data system was \code{L2261.regional_biomass_CHINA.R} (gcam-china level2).
#' @details Create biomass supply sectors at the provincial level, in order ensure that biomass carbon-tracking is
#' contained entirely within the consuming region (province).
#' @importFrom assertthat assert_that
#' @importFrom dplyr distinct filter inner_join if_else mutate select semi_join
#' @importFrom tibble tibble
#' @author MTB Aug 2018
module_gcamchina_L2261.regional_biomass_CHINA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A21.sector",
             FILE = "energy/A26.sector",
             FILE = "gcam-china/A28.sector",
             FILE = 'gcam-china/A23.elecS_tech_mapping_cool',
             FILE = "energy/calibrated_techs",
             FILE = "gcam-china/china_seawater_states_basins",
             "L122.out_EJ_province_refining_F",
             "L202.CarbonCoef",
             "L221.GlobalTechCoef_en",
             "L221.StubTechCoef_bioOil",
             "L221.SubsectorInterp_en",
             "L221.StubTech_en",
             "L221.StubTechShrwt_bioOil",
             "L221.StubTechFractProd_en",
             "L221.StubTechFractSecOut_en",
             "L221.Rsrc_en",
             "L221.RsrcPrice_en",
             "L226.SubsectorInterp_en",
             "L226.GlobalTechEff_en",
             "L226.GlobalTechCost_en",
             "L222.StubTechMarket_en_CHINA",
             "L2234.StubTechMarket_elecS_CHINA",
             "L232.StubTechMarket_ind_CHINA",
             "L2321.StubTechMarket_cement_CHINA",
             "L244.StubTechMarket_CHINAbld",
             "L221.StubTechFractCalPrice_en",
             "L221.StubTechCalInput_bioOil",
             "L221.StubTechInterp_bioOil"))
  } else if(command == driver.DECLARE_OUTPUTS) {
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
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # silence check package notes
    from.year <- input.cost <- input.unit <- logit.exponent <- logit.type <- market.name <-
      minicam.non.energy.input <- output.unit <- price.unit <- region <- sector.name <- state <-
      stub.technology <- subsector <- supplysector <- technology <- to.year <- year <- traded <-
      subsector.name <- share.weight <- fractional.secondary.output <- price <- fraction.produced <-
      PrimaryFuelCO2Coef.name <- PrimaryFuelCO2Coef <- minicam.energy.input <- sector <- calibrated.value <-
      value <- share <- fuel <- . <- output.ratio <- subs.share.weight <- tech.share.weight <-
      subsector_1 <- to.technology <- NULL

    # import data
    A21.sector <- get_data(all_data, "energy/A21.sector", strip_attributes = TRUE)
    A26.sector <- get_data(all_data, "energy/A26.sector", strip_attributes = TRUE)
    A28.sector <- get_data(all_data, "gcam-china/A28.sector", strip_attributes = TRUE) # copy from gcam-usa
    A23.elecS_tech_mapping_cool <- get_data(all_data, "gcam-china/A23.elecS_tech_mapping_cool", strip_attributes = TRUE) # copy from usa
    china_seawater_states_basins <- get_data(all_data, "gcam-china/china_seawater_states_basins", strip_attributes = TRUE) # revised from usa, need to check
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs", strip_attributes = TRUE)
    A21.sector <- get_data(all_data, "energy/A21.sector", strip_attributes = TRUE)
    A26.sector <- get_data(all_data, "energy/A26.sector", strip_attributes = TRUE)
    A28.sector <- get_data(all_data, "gcam-china/A28.sector", strip_attributes = TRUE)
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    L122.out_EJ_province_refining_F <- get_data(all_data, "L122.out_EJ_province_refining_F", strip_attributes = TRUE)
    L202.CarbonCoef <- get_data(all_data, "L202.CarbonCoef", strip_attributes = TRUE)
    L221.GlobalTechCoef_en <- get_data(all_data, "L221.GlobalTechCoef_en", strip_attributes = TRUE)
    #Adding in country level co-efficients for regional oil crop
    L221.StubTechCoef_bioOil <- get_data(all_data, "L221.StubTechCoef_bioOil", strip_attributes = TRUE)
    L221.SubsectorInterp_en <- get_data(all_data, "L221.SubsectorInterp_en", strip_attributes = TRUE)
    L221.StubTech_en <- get_data(all_data, "L221.StubTech_en", strip_attributes = TRUE)
    L221.StubTechShrwt_bioOil <- get_data(all_data, "L221.StubTechShrwt_bioOil", strip_attributes = TRUE)
    L221.StubTechFractProd_en <- get_data(all_data, "L221.StubTechFractProd_en", strip_attributes = TRUE)
    L221.StubTechFractSecOut_en <- get_data(all_data, "L221.StubTechFractSecOut_en", strip_attributes = TRUE)
    L221.Rsrc_en <- get_data(all_data, "L221.Rsrc_en", strip_attributes = TRUE)
    L221.RsrcPrice_en <- get_data(all_data, "L221.RsrcPrice_en", strip_attributes = TRUE)
    L226.SubsectorInterp_en <- get_data(all_data, "L226.SubsectorInterp_en", strip_attributes = TRUE)
    L226.GlobalTechEff_en <- get_data(all_data, "L226.GlobalTechEff_en", strip_attributes = TRUE)
    L226.GlobalTechCost_en <- get_data(all_data, "L226.GlobalTechCost_en", strip_attributes = TRUE)
    L222.StubTechMarket_en_CHINA <- get_data(all_data, "L222.StubTechMarket_en_CHINA", strip_attributes = TRUE)
    L2234.StubTechMarket_elecS_CHINA <- get_data(all_data, "L2234.StubTechMarket_elecS_CHINA", strip_attributes = TRUE)
    L232.StubTechMarket_ind_CHINA <- get_data(all_data, "L232.StubTechMarket_ind_CHINA", strip_attributes = TRUE)
    L2321.StubTechMarket_cement_CHINA <- get_data(all_data, "L2321.StubTechMarket_cement_CHINA", strip_attributes = TRUE)
    L244.StubTechMarket_bld <- get_data(all_data, "L244.StubTechMarket_CHINAbld", strip_attributes = TRUE) # L244.StubTechMarket_bld
    L221.StubTechFractCalPrice_en <- get_data(all_data, "L221.StubTechFractCalPrice_en", strip_attributes = TRUE)
    L221.StubTechCalInput_bioOil <- get_data(all_data, "L221.StubTechCalInput_bioOil", strip_attributes = TRUE)
    L221.StubTechInterp_bioOil <- get_data(all_data, "L221.StubTechInterp_bioOil", strip_attributes = TRUE)

    # Data Processing

    # Supply Sectors and Subsectors
    # Deleting China-level biomass sector
    # Not deleting China regional biomass because gas processing & H2 central production
    # still both occur at the China level and consume China regional biomass

    A28.sector %>%
      filter(supplysector != "regional biomass") %>%
      mutate(region = gcamchina.REGION) %>%
      select(region, supplysector) -> L2261.DeleteSupplysector_bio_CHINA

    L2261.CHINA_biomass_sectors <- unique(A28.sector$supplysector)

    A21.sector %>%
      bind_rows(A26.sector) %>%
      filter(supplysector %in% L2261.CHINA_biomass_sectors) %>%
      repeat_add_columns(tibble(region = gcamchina.PROVINCES_ALL)) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES$Supplysector, logit.type) -> L2261.Supplysector_bio_CHINA

    # Subsector shareweights of state-level biomass sectors
    L2261.Supplysector_bio_CHINA %>%
      mutate(subsector = supplysector,
             year.fillout = min(MODEL_BASE_YEARS),
             share.weight = gcamchina.DEFAULT_SHAREWEIGHT) %>%
      select(LEVEL2_DATA_NAMES$SubsectorShrwtFllt) -> L2261.SubsectorShrwtFllt_bio_CHINA

    # Subsector shareweight interpolations for state-level biomass sectors
    L221.SubsectorInterp_en %>%
      bind_rows(L226.SubsectorInterp_en) %>%
      filter(region == gcamchina.REGION) %>%
      # filter for biomass supply sectors
      semi_join(A28.sector, by = c("supplysector")) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamchina.PROVINCES_ALL)) %>%
      select(LEVEL2_DATA_NAMES$SubsectorInterp) -> L2261.SubsectorInterp_bio_CHINA

    # NOTE: There is only one tech per subsector so the logit choice does not matter
    L2261.SubsectorShrwtFllt_bio_CHINA %>%
      select(LEVEL2_DATA_NAMES$Subsector) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = gcamchina.DEFAULT_LOGITEXP,
             logit.type = gcamchina.DEFAULT_LOGIT_TYPE) -> L2261.SubsectorLogit_bio_CHINA

    # Stub-technologies for biomass sectors that consume from global markets
    # Stub-technologies for state-level biomass sector
    L221.StubTech_en %>%
      filter(region == gcamchina.REGION) %>%
      # filter for biomass supply sectors
      semi_join(A28.sector, by = c("supplysector")) %>%
      # NOTE: can't use stub technology for state-level regional biomass sectors
      # because they would inherit the wrong energy-inputs
      filter(stub.technology != "regional biomass") %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamchina.PROVINCES_ALL)) %>%
      select(LEVEL2_DATA_NAMES$StubTech) -> L2261.StubTech_bio_CHINA

    # Technology inputs & markets of state-level biomass sectors
    #kbn 2020-
    L221.GlobalTechCoef_en %>%
      rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) %>%
      semi_join(L2261.StubTech_bio_CHINA, by = c("supplysector", "subsector", "stub.technology")) %>%
      repeat_add_columns(tibble(region = gcamchina.PROVINCES_ALL)) %>%
      mutate(market.name = gcamchina.REGION) %>%
      select(LEVEL2_DATA_NAMES$StubTechMarket) -> L2261.StubTechMarket_bio_CHINA

    # kbn 2020
    L221.StubTechCoef_bioOil %>%
      filter(region=="CHINA") %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamchina.PROVINCES_ALL)) %>%
      mutate(market.name = gcamchina.REGION) %>%
      select(LEVEL2_DATA_NAMES$StubTechCoef)->L2261.StubTechCoef_bioOil_CHINA


    # Technology share-weights of state-level regional biomassOil sectors
    # kbn 2020-03-29 No longer writing out share-weights for some regions. But we need this for GCAM-USA
    L221.StubTechShrwt_bioOil %>%
      # mutate that does nothing to ensure prior metadata is dropped
      mutate(region = region) %>%
      filter(region == gcamchina.REGION) -> L2261.StubTechShrwt_rbO_CHINA

    if(nrow(L2261.StubTechShrwt_rbO_CHINA) > 0) {
      L2261.StubTechShrwt_rbO_CHINA %>%
        select(-region) %>%
        repeat_add_columns(tibble(region = gcamchina.PROVINCES_ALL)) %>%
        select(LEVEL2_DATA_NAMES$StubTechYr, share.weight) -> L2261.StubTechShrwt_rbO_CHINA
    }

    # Secondary (feed) outputs of global technologies for upstream (bio)energy
    # NOTE: secondary outputs are only considered in future time periods
    # NOTE: secondary outputs are only written for the regions/technologies where applicable,
    # so the global tech database can not be used
    L221.StubTechFractSecOut_en %>%
      filter(region == gcamchina.REGION) %>%
      # filter for biomass supply sectors
      semi_join(A28.sector, by = c("supplysector")) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamchina.PROVINCES_ALL)) %>%
      select(LEVEL2_DATA_NAMES$StubTechFractSecOut) -> L2261.StubTechFractSecOut_bio_CHINA


    # Cost curve points for producing secondary output feedcrops
    L221.StubTechFractProd_en %>%
      filter(region == gcamchina.REGION) %>%
      # filter for biomass supply sectors
      semi_join(A28.sector, by = c("supplysector")) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamchina.PROVINCES_ALL)) %>%
      select(LEVEL2_DATA_NAMES$StubTechYr, fractional.secondary.output, price, fraction.produced) -> L2261.StubTechFractProd_bio_CHINA

    L221.StubTechFractCalPrice_en %>%
      filter(region == gcamchina.REGION) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamchina.PROVINCES_ALL)) %>%
      select(LEVEL2_DATA_NAMES$StubTechFractCalPrice) -> L2261.StubTechFractCalPrice_bio_CHINA


    calibrated_techs %>%
      # filter for biomass supply sectors
      semi_join(A28.sector, by=c("minicam.energy.input" = "supplysector")) %>%
      select(sector, minicam.energy.input) %>%
      # filter for biomass energy only
      inner_join(L122.out_EJ_province_refining_F, c("sector")) %>%
      #filter(value != 0) %>%
      select(-sector, -fuel) %>%
      group_by(minicam.energy.input, year) %>%
      mutate(share = value / sum(value),
             share = if_else(is.na(share), 0, share)) %>%
      ungroup() %>%
      rename(supplysector = minicam.energy.input) %>%
      # expand L221.StubTechCalInput_bioOil to the states, adjust calibrated.value by the state shares
      left_join(filter(L221.StubTechCalInput_bioOil, region == gcamchina.REGION), ., by=c("supplysector", "year")) %>%
      mutate(calibrated.value = share * calibrated.value,
             region = province) %>%
      select(LEVEL2_DATA_NAMES$StubTechCalInput) -> L2261.StubTechCalInput_bio_CHINA

    L221.StubTechInterp_bioOil %>%
      filter(region == gcamchina.REGION) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamchina.PROVINCES_ALL)) %>%
      select(LEVEL2_DATA_NAMES$StubTechInterp) -> L2261.StubTechInterp_bio_CHINA

    # Connecting state-level DDGS & feedcakes secondary outputs to USA sector
    # depletable resource info for state-level DDGS & feedcake secondary outputs
    L221.Rsrc_en %>%
      filter(region == gcamchina.REGION) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamchina.PROVINCES_ALL)) %>%
      select(LEVEL2_DATA_NAMES$Rsrc) -> L2261.Rsrc_DDGS_CHINA

    # Depletable resource prices for state-level DDGS & feedcake secondary outputs
    L221.RsrcPrice_en %>%
      filter(region == gcamchina.REGION) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamchina.PROVINCES_ALL)) %>%
      select(LEVEL2_DATA_NAMES$RsrcPrice) -> L2261.RsrcPrice_DDGS_CHINA

    # Technologies for provincial-level regional biomass sectors, which consume "regional biomass" from China regional biomass sector
    # NOTE: can't use stub technology for provincial-level regional biomass sectors because they would inherit the wrong energy-inputs
    L221.StubTech_en %>%
      filter(region == gcamchina.REGION,
             stub.technology == "regional biomass") %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamchina.PROVINCES_ALL)) %>%
      rename(technology = stub.technology) %>%
      select(LEVEL2_DATA_NAMES$Tech) -> L2261.Tech_rbm_CHINA

    # Technology shareweights of provincial-level regional biomass sectors
    L2261.Tech_rbm_CHINA %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = gcamchina.DEFAULT_SHAREWEIGHT) %>%
      select(LEVEL2_DATA_NAMES$TechYr, share.weight) -> L2261.TechShrwt_rbm_CHINA

    # Technology input & efficiencies of state-level regional biomass sectors
    # State-level regional biomass sectors consume "regional biomass" from USA regional biomass sector
    # This is done to avoid any potential conflict with the bio trade feature
    L2261.TechShrwt_rbm_CHINA %>%
      select(LEVEL2_DATA_NAMES$TechYr) %>%
      mutate(minicam.energy.input = "regional biomass",
             coefficient = gcamchina.DEFAULT_COEFFICIENT,
             market.name = gcamchina.REGION) %>%
      select(LEVEL2_DATA_NAMES$TechCoef) -> L2261.TechCoef_rbm_CHINA

    # Technologies for delivered biomass sectors, which consume from provincial-level markets
    # NOTE: can't use stub technology for delivered biomass sectors because they would inherit the wrong energy-inputs
    # Technologies for provincial-level delivered biomass sector
    L226.GlobalTechEff_en %>%
      # filter for biomass supply sectors
      semi_join(A28.sector, by = c("sector.name" = "supplysector")) %>%
      rename(supplysector = sector.name, subsector = subsector.name) %>%
      #repeat_add_columns(tibble(region = gcamchina.REGION)) %>%
      repeat_add_columns(tibble(region = gcamchina.PROVINCES_ALL)) %>%
      select(LEVEL2_DATA_NAMES$Tech) %>%
      distinct() -> L2261.Tech_dbm_CHINA

    # Technology shareweights of state-level delivered biomass sectors
    L2261.Tech_dbm_CHINA %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = gcamchina.DEFAULT_SHAREWEIGHT) %>%
      select(LEVEL2_DATA_NAMES$TechYr, share.weight) -> L2261.TechShrwt_dbm_CHINA

    # Technology efficiencies of provincial-level delivered biomass sectors
    L226.GlobalTechEff_en %>%
      # filter for biomass supply sectors
      semi_join(A28.sector, by = c("sector.name" = "supplysector")) %>%
      rename(supplysector = sector.name, subsector = subsector.name) %>%
      #repeat_add_columns(tibble(region = gcamchina.REGION)) %>%
      repeat_add_columns(tibble(region = gcamchina.PROVINCES_ALL)) %>%
      mutate(market.name = region) %>%
      select(LEVEL2_DATA_NAMES$TechEff) -> L2261.TechEff_dbm_CHINA

    # Technology costs for state-level delivered biomass sectors
    L226.GlobalTechCost_en %>%
      # filter for biomass supply sectors
      semi_join(A28.sector, by = c("sector.name" = "supplysector")) %>%
      rename(supplysector = sector.name, subsector = subsector.name)  %>%
      repeat_add_columns(tibble(region = gcamchina.PROVINCES_ALL)) %>%
      select(LEVEL2_DATA_NAMES$TechCost) -> L2261.TechCost_dbm_CHINA

    # Carbon coefficients for state-level biomass sectors
    L202.CarbonCoef %>%
      filter(region == gcamchina.REGION,
             PrimaryFuelCO2Coef.name %in% c(L2261.CHINA_biomass_sectors)) %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamchina.PROVINCES_ALL)) %>%
      select(region, PrimaryFuelCO2Coef.name, PrimaryFuelCO2Coef) -> L2261.CarbonCoef_bio_CHINA

    # Adjusting the carbon coefficient for China regional biomass
    # This is necessary because, in order to ensure compatibility with the
    # bio trade and negative emissions budget features, US states can't consume biomass
    # from the global market, but rather consume regional biomass from the China market.
    # In order to associate the sequestered carbon embedded in biomass feedstocks with
    # the state in which they're consumed (rather than the China region), we set the
    # China regional biomass carbon coefficient to zero
    L202.CarbonCoef %>%
      filter(region == gcamchina.REGION,
             PrimaryFuelCO2Coef.name == "regional biomass") %>%
      mutate(PrimaryFuelCO2Coef = 0) %>%
      select(region, PrimaryFuelCO2Coef.name, PrimaryFuelCO2Coef) -> L2261.CarbonCoef_rbm_CHINA
    L2261.CarbonCoef_rbm_CHINA %>%
      bind_rows(L2261.CarbonCoef_bio_CHINA) -> L2261.CarbonCoef_bio_CHINA

    # Update minicam-energy-inputs for technologies that consume biomass
    set_state_biomass_markets <- function( data ){
      data_new <- data %>%
        filter(region %in% gcamchina.PROVINCES_ALL,
               minicam.energy.input %in% L2261.CHINA_biomass_sectors) %>%
        mutate(market.name  = region)
      return( data_new)
    }

    # Energy Transformation (Refining)
    L2261.StubTechMarket_en_CHINA <- set_state_biomass_markets(L222.StubTechMarket_en_CHINA)

    # Electricity (load segments)
    L2261.StubTechMarket_elecS_CHINA <- set_state_biomass_markets(L2234.StubTechMarket_elecS_CHINA)

    # Industry
    L2261.StubTechMarket_ind_CHINA <- set_state_biomass_markets(L232.StubTechMarket_ind_CHINA)

    # Cement
    L2261.StubTechMarket_cement_CHINA <- set_state_biomass_markets(L2321.StubTechMarket_cement_CHINA)

    # Buildings
    L2261.StubTechMarket_bld_CHINA <- set_state_biomass_markets(L244.StubTechMarket_bld)

    ## To account for new nesting-subsector structure and to add cooling technologies, we must expand certain outputs

    # Define unique states and basins that have access to seawater that will
    # allow for seawate cooling

    seawater_states_basins <- unique(china_seawater_states_basins$seawater_region)

    add_cooling_techs <- function(data){
      data %>%
        left_join(A23.elecS_tech_mapping_cool,
                  by=c("stub.technology"="Electric.sector.technology",
                       "supplysector"="Electric.sector","subsector")) %>%
        select(-technology,-subsector_1)%>%
        rename(technology = to.technology,
               subsector0 = subsector,
               subsector = stub.technology) -> data_new

      data_new %>% filter(grepl(gcamchina.WIND_BASE_COST_YEAR,technology)) %>% filter((region %in% seawater_states_basins)) %>%
        bind_rows(data_new %>% filter(!grepl(gcamchina.WIND_BASE_COST_YEAR,technology))) %>%
        arrange(region,year) -> data_new
      return(data_new)
    }
    L2261.StubTechMarket_elecS_CHINA <- add_cooling_techs(L2261.StubTechMarket_elecS_CHINA)

    # ===================================================
    # Produce outputs

    L2261.DeleteSupplysector_bio_CHINA %>%
      add_title("Delete CHINA-level Biomass Sectors") %>%
      add_units("NA") %>%
      add_comments("CHINA regional biomass not deleted") %>%
      add_comments("CHINA gas processing & H2 central production sectors still consume CHINA regional biomass") %>%
      add_legacy_name("L2261.DeleteSupplysector_bio_CHINA") %>%
      add_precursors("gcam-china/A28.sector") ->
      L2261.DeleteSupplysector_bio_CHINA

    L2261.Supplysector_bio_CHINA %>%
      add_title("province-level Biomass Supply Sector Information") %>%
      add_units("unitless") %>%
      add_comments("Supply sector information for province-level biomass supply sectors") %>%
      add_legacy_name("L2261.Supplysector_bio_CHINA") %>%
      add_precursors("energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-china/A28.sector") ->
      L2261.Supplysector_bio_CHINA

    L2261.SubsectorShrwtFllt_bio_CHINA %>%
      add_title("Subsector Shareweights for State-level Biomass Supply Sectors") %>%
      add_units("unitless") %>%
      add_comments("Subsector shareweights for state-level biomass supply sectors") %>%
      add_legacy_name("L2261.SubsectorShrwtFllt_bio_CHINA") %>%
      same_precursors_as("L2261.Supplysector_bio_CHINA") ->
      L2261.SubsectorShrwtFllt_bio_CHINA


    L2261.SubsectorInterp_bio_CHINA %>%
      add_title("Subsector Shareweights for State-level Biomass Supply Sectors") %>%
      add_units("NA") %>%
      add_comments("Subsector shareweight interpolations for state-level biomass supply sectors") %>%
      add_legacy_name("L2261.SubsectorInterp_bio_CHINA") %>%
      add_precursors("gcam-china/A28.sector",
                     "L221.SubsectorInterp_en",
                     "L226.SubsectorInterp_en") ->
      L2261.SubsectorInterp_bio_CHINA


    L2261.SubsectorLogit_bio_CHINA %>%
      add_title("Subsector Logits for State-level Biomass Supply Sectors") %>%
      add_units("unitless") %>%
      add_comments("Subsector logits for state-level biomass supply sectors") %>%
      add_comments("There is only one tech per subsector so the logit choice does not matter") %>%
      add_legacy_name("L2261.SubsectorLogit_bio_CHINA") %>%
      same_precursors_as("L2261.Supplysector_bio_CHINA") ->
      L2261.SubsectorLogit_bio_CHINA


    L2261.StubTech_bio_CHINA %>%
      add_title("State-level Biomass Supply Sector Stub Technologies") %>%
      add_units("NA") %>%
      add_comments("Stub-technologies for state-level biomass supply sectors") %>%
      add_legacy_name("L2261.StubTech_bio_CHINA") %>%
      add_precursors("gcam-china/A28.sector",
                     "L221.StubTech_en") ->
      L2261.StubTech_bio_CHINA

    L2261.StubTechMarket_bio_CHINA %>%
      add_title("Technology Market Information for State-level Biomass Supply Sectors") %>%
      add_units("NA") %>%
      add_comments("Technology inputs and markets for state-level biomass supply sectors") %>%
      add_legacy_name("L2261.StubTechMarket_bio_CHINA") %>%
      add_precursors("gcam-china/A28.sector",
                     "L221.GlobalTechCoef_en",
                     "L221.StubTech_en") ->
      L2261.StubTechMarket_bio_CHINA

    L2261.StubTechCoef_bioOil_CHINA %>%
      add_title("Technology coefficients specific crops at province-level Biomass Supply Sectors") %>%
      add_units("NA") %>%
      add_comments("Technology coefficients specific crops at State-level Biomass Supply Sectors") %>%
      add_legacy_name("L2261.StubTechCoef_bioOil_CHINA") %>%
      add_precursors("gcam-china/A28.sector",
                     "L221.StubTechCoef_bioOil") ->L2261.StubTechCoef_bioOil_CHINA

    L2261.StubTechShrwt_rbO_CHINA %>%
      add_title("Technology Shareweights for province-level Regional Biomass Oil Supply Sectors") %>%
      add_units("unitless") %>%
      add_comments("Technology share-weights for province-level regional biomassOil supply sectors") %>%
      add_legacy_name("L2261.StubTechShrwt_rbO_CHINA") %>%
      add_precursors("L221.StubTechShrwt_bioOil") ->
      L2261.StubTechShrwt_rbO_CHINA

    L2261.StubTechFractSecOut_bio_CHINA %>%
      add_title("Secondary Feed Outputs of State-level Biomass Supply Sectors") %>%
      add_units("fractions") %>%
      add_comments("Secondary output (DDGS and feedcakes) generated from corn and biomassOil only") %>%
      add_comments("Secondary outputs are only considered in future time periods") %>%
      add_legacy_name("L2261.StubTechFractSecOut_bio_CHINA") %>%
      add_precursors("gcam-china/A28.sector",
                     "L221.StubTechFractSecOut_en") ->
      L2261.StubTechFractSecOut_bio_CHINA

    L2261.StubTechFractProd_bio_CHINA %>%
      add_title("Production Information for province-level Biomass Supply Sector Secondary Feed Outputs") %>%
      add_units("1975$ (price); fraction") %>%
      add_comments("Cost curve points (prices and production fraction) for producing secondary output feedcrops") %>%
      add_comments("Secondary output (DDGS and feedcakes) generated from corn and biomassOil only") %>%
      add_legacy_name("L2261.StubTechFractProd_bio_CHINA") %>%
      add_precursors("gcam-china/A28.sector",
                     "L221.StubTechFractProd_en") ->
      L2261.StubTechFractProd_bio_CHINA

    L2261.StubTechFractCalPrice_bio_CHINA %>%
      add_title("Calibrated historical price for DDGS") %>%
      add_units("1975$/kg") %>%
      add_comments("Value only relevant for share-weight calculation") %>%
      add_precursors("L221.StubTechFractCalPrice_en") ->
      L2261.StubTechFractCalPrice_bio_CHINA

    L2261.StubTechCalInput_bio_CHINA %>%
      add_title("Calibrated output of biomassOil by feedstock type") %>%
      add_units("Mt/yr") %>%
      add_comments("Calibration is necessary to allow regions to have multiple biomassOil feedstocks") %>%
      add_precursors("energy/calibrated_techs",
                     "gcam-china/A28.sector",
                     "L122.out_EJ_province_refining_F",
                     "L221.StubTechCalInput_bioOil") ->
      L2261.StubTechCalInput_bio_CHINA

    L2261.StubTechInterp_bio_CHINA %>%
      add_title("biomassOil technology (feedstock type) shareweight interpolation") %>%
      add_units("unitless") %>%
      add_comments("Regions with multiple feedstocks in the base year have their share-weights passed forward") %>%
      add_precursors("L221.StubTechInterp_bioOil") ->
      L2261.StubTechInterp_bio_CHINA

    L2261.Rsrc_DDGS_CHINA %>%
      add_title("Depletable Resource Information for State-level Biomass Supply Sector Secondary Feed Outputs") %>%
      add_units("NA") %>%
      add_comments("Depletable resource info for state-level DDGS & feedcake secondary outputs") %>%
      add_legacy_name("L2261.Rsrc_DDGS_CHINA") %>%
      add_precursors("L221.Rsrc_en") ->
      L2261.Rsrc_DDGS_CHINA

    L2261.RsrcPrice_DDGS_CHINA %>%
      add_title("Depletable Resource Information for State-level Biomass Supply Sector Secondary Feed Outputs") %>%
      add_units("1975$/kg") %>%
      add_comments("Depletable resource prices for state-level DDGS & feedcake secondary outputs") %>%
      add_legacy_name("L2261.RsrcPrice_DDGS_CHINA") %>%
      add_precursors("L221.RsrcPrice_en") ->
      L2261.RsrcPrice_DDGS_CHINA

    L2261.Tech_rbm_CHINA %>%
      add_title("Province-level Regional Biomass Technologies") %>%
      add_units("NA") %>%
      add_comments("Technologies for province-level regional biomass sector") %>%
      add_comments("Can't use stub-technology tag for regional biomass sectors because they would inherit the wrong energy-inputs") %>%
      add_precursors("L221.StubTech_en") ->
      L2261.Tech_rbm_CHINA

    L2261.TechShrwt_rbm_CHINA%>%
      add_title("Technology Shareweights for Province-level Regional Biomass Sectors") %>%
      add_units("unitless") %>%
      add_comments("Technology shareweights for Province-level regional biomass sectors") %>%
      same_precursors_as("L2261.Tech_rbm_CHINA") ->
      L2261.TechShrwt_rbm_CHINA

    L2261.TechCoef_rbm_CHINA%>%
      add_title("Technology Market Info for Province-level Regional Biomass Sectors") %>%
      add_units("unitless") %>%
      add_comments("Technology market info and coefficients for province-level regional biomass sectors") %>%
      same_precursors_as("L2261.Tech_rbm_CHINA") ->
      L2261.TechCoef_rbm_CHINA

    L2261.Tech_dbm_CHINA %>%
      add_title("Province-level Delivered Biomass Technologies") %>%
      add_units("NA") %>%
      add_comments("Technologies for province-level delivered biomass sector") %>%
      add_comments("Can't use stub-technology tag for delivered biomass sectors because they would inherit the wrong energy-inputs") %>%
      add_legacy_name("L2261.Tech_dbm_CHINA") %>%
      add_precursors("gcam-china/A28.sector",
                     "L226.GlobalTechEff_en") ->
      L2261.Tech_dbm_CHINA

    L2261.TechShrwt_dbm_CHINA %>%
      add_title("Technology Shareweights for Province-level Delivered Biomass Sectors") %>%
      add_units("unitless") %>%
      add_comments("Technology shareweights for province-level delivered biomass sectors") %>%
      add_legacy_name("L2261.TechShrwt_dbm_CHINA") %>%
      same_precursors_as("L2261.Tech_dbm_CHINA") ->
      L2261.TechShrwt_dbm_CHINA

    L2261.TechEff_dbm_CHINA %>%
      add_title("Technology Efficiencies for province-level Delivered Biomass Sectors") %>%
      add_units("unitless") %>%
      add_comments("Technology efficiencies for province-level delivered biomass sectors") %>%
      add_legacy_name("L2261.TechEff_dbm_CHINA") %>%
      add_precursors("gcam-china/A28.sector",
                     "L226.GlobalTechEff_en") ->
      L2261.TechEff_dbm_CHINA

    L2261.TechCost_dbm_CHINA %>%
      add_title("Technology Costs for Province-level Delivered Biomass Sectors") %>%
      add_units("1975$/GJ") %>%
      add_comments("Technology costs for province-level delivered biomass sectors") %>%
      add_legacy_name("L2261.TechCost_dbm_CHINA") %>%
      add_precursors("gcam-china/A28.sector",
                     "L226.GlobalTechCost_en") ->
      L2261.TechCost_dbm_CHINA

    L2261.CarbonCoef_bio_CHINA %>%
      add_title("Primary Energy CO2 Coefficient") %>%
      add_units("kgC/GJ") %>%
      add_comments("Carbon coefficients for province-level biomass sectors") %>%
      add_legacy_name("L2261.CarbonCoef_bio_CHINA") %>%
      add_precursors("gcam-china/A28.sector",
                     "L202.CarbonCoef") ->
      L2261.CarbonCoef_bio_CHINA

    L2261.StubTechMarket_en_CHINA %>%
      add_title("Market Information for Province-level Refining Sectors") %>%
      add_units("NA") %>%
      add_comments("Updating market information for biomass inputs to province-level refining sectors") %>%
      add_comments("Now consuming from province-level biomass supply sectors") %>%
      add_precursors("gcam-china/A28.sector",
                     "L222.StubTechMarket_en_CHINA") ->
      L2261.StubTechMarket_en_CHINA

    L2261.StubTechMarket_elecS_CHINA %>%
      add_title("Market Information for Province-level (multiple load segment) Electricity Sectors") %>%
      add_units("NA") %>%
      add_comments("Updating market information for biomass inputs to province-level electricity sectors") %>%
      add_comments("Now consuming from province-level biomass supply sectors") %>%
      add_precursors("gcam-china/A28.sector",
                     'gcam-china/A23.elecS_tech_mapping_cool',
                     "gcam-china/china_seawater_states_basins",
                     "L2234.StubTechMarket_elecS_CHINA") ->
      L2261.StubTechMarket_elecS_CHINA

    L2261.StubTechMarket_ind_CHINA %>%
      add_title("Market Information for Province-level Industrial Energy Use Sectors") %>%
      add_units("NA") %>%
      add_comments("Updating market information for biomass inputs to province-level industrial energy use sectors") %>%
      add_comments("Now consuming from province-level biomass supply sectors") %>%
      add_precursors("gcam-china/A28.sector",
                     "L232.StubTechMarket_ind_CHINA") ->
      L2261.StubTechMarket_ind_CHINA

    L2261.StubTechMarket_cement_CHINA %>%
      add_title("Market Information for Province-level Process Heat Cement Sectors") %>%
      add_units("NA") %>%
      add_comments("Updating market information for biomass inputs to province-level process heat cement sectors") %>%
      add_comments("Now consuming from province-level biomass supply sectors") %>%
      add_precursors("gcam-china/A28.sector",
                     "L2321.StubTechMarket_cement_CHINA") ->
      L2261.StubTechMarket_cement_CHINA

    L2261.StubTechMarket_bld_CHINA %>%
      add_title("Market Information for Province-level Building Sectors") %>%
      add_units("NA") %>%
      add_comments("Updating market information for biomass inputs to province-level building sectors") %>%
      add_comments("Now consuming from province-level biomass supply sectors") %>%
      add_precursors("gcam-china/A28.sector",
                     "L244.StubTechMarket_CHINAbld") ->
      L2261.StubTechMarket_bld_CHINA

    return_data(L2261.DeleteSupplysector_bio_CHINA,
                L2261.Supplysector_bio_CHINA,
                L2261.SubsectorShrwtFllt_bio_CHINA,
                L2261.SubsectorInterp_bio_CHINA,
                L2261.SubsectorLogit_bio_CHINA,
                L2261.StubTech_bio_CHINA,
                L2261.StubTechMarket_bio_CHINA,
                L2261.StubTechCoef_bioOil_CHINA,
                L2261.StubTechShrwt_rbO_CHINA,
                L2261.StubTechFractSecOut_bio_CHINA,
                L2261.StubTechFractProd_bio_CHINA,
                L2261.StubTechFractCalPrice_bio_CHINA,
                L2261.StubTechCalInput_bio_CHINA,
                L2261.StubTechInterp_bio_CHINA,
                L2261.Rsrc_DDGS_CHINA,
                L2261.RsrcPrice_DDGS_CHINA,
                L2261.Tech_rbm_CHINA,
                L2261.TechShrwt_rbm_CHINA,
                L2261.TechCoef_rbm_CHINA,
                L2261.Tech_dbm_CHINA,
                L2261.TechShrwt_dbm_CHINA,
                L2261.TechEff_dbm_CHINA,
                L2261.TechCost_dbm_CHINA,
                L2261.CarbonCoef_bio_CHINA,
                L2261.StubTechMarket_en_CHINA,
                L2261.StubTechMarket_elecS_CHINA,
                L2261.StubTechMarket_ind_CHINA,
                L2261.StubTechMarket_cement_CHINA,
                L2261.StubTechMarket_bld_CHINA)
  } else {
    stop("Unknown command")
  }
}
