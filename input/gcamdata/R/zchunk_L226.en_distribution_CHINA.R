#' module_gcam.china_L226.en_distribution_CHINA
#'
#' Create a variety of energy and electricity outputs for CHINA at the province and/or grid.region level.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:
#' \itemize{
#' \item{\code{L226.DeleteSupplysector_CHINAelec}: Removing the electricity T&D sectors of the CHINA region.}
#' \item{\code{L226.StubTechCoef_electd_CHINA}: Stub technology coefficients elec T&D when using national elec markets. State elect_td sectors are treated as stub technologies.}
#' \item{\code{L226.TechShrwt_electd_CHINA}: tech share weights for elec T&D when using regional electricity markets. The elect_td sectors can not use the global tech database as their input is different.}
#' \item{\code{L226.TechCost_electd_CHINA}: Tech costs for elec T&D when using regional electricity markets.}
#' \item{\code{L226.Supplysector_electd_CHINA}: CHINA supply sector input, output, and logit info for elec T&D by province.}
#' \item{\code{L226.SubsectorShrwtFllt_electd_CHINA}: CHINA subsector shareweight fillout for elec T&D by state.}
#' \item{\code{L226.SubsectorLogit_electd_CHINA}: CHINA subsector logit info for elec T&D by grid.region.}
#' \item{\code{L226.SubsectorInterp_electd_CHINA}: CHINA interpolation info for elec T&D by province.}
#' \item{\code{L226.TechCoef_electd_CHINA}: Tech coeff for elec T&D when using regional electricity markets.}
#' \item{\code{L226.TechShrwt_en_CHINA}: Technology shareweights of energy handling and delivery. Can't use stub technologies because these would inherit the wrong energy-inputs.}
#' \item{\code{L226.TechCost_en_CHINA}: Regional price adjustments/cost adders for CHINA energy.}
#' \item{\code{L226.TechCoef_en_CHINA}: Technology coefficients and market names of energy handling and delivery.}
#' \item{\code{L226.Supplysector_en_CHINA}: Supply sector information for energy handling and delivery sectors for CHINA grid regions.}
#' \item{\code{L226.SubsectorShrwtFllt_en_CHINA}: Subsector shareweights of energy handling and delivery.}
#' \item{\code{L226.SubsectorLogit_en_CHINA}: Logit info for energy subsectors.}
#' \item{\code{L226.Ccoef_CHINA}: Carbon coef for CHINA cost adder sectors.}
#' }
#' The corresponding file in the original data system was \code{L226.en_distribution_CHINA.R} (gcam-china level2).
#' @details Create a variety of energy and electricity outputs for CHINA at the province and/or grid.region level.
#' @importFrom dplyr bind_rows distinct filter if_else group_by left_join mutate select summarize
#' @importFrom tidyr gather spread
#' @author LuRen Dec 2019, edited by BY Feb 2020

module_gcam.china_L226.en_distribution_CHINA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "energy/A21.sector",
             FILE = "energy/A26.sector",
             FILE = "gcam-china/regional_fuel_prices_RMB",
             "L202.CarbonCoef",
             "L101.NBS_use_all_Mtce",
             "L226.Supplysector_en",
             "L226.SubsectorLogit_en",
             #"L226.SubsectorShrwt_en",
             "L226.SubsectorShrwtFllt_en",
             "L226.SubsectorInterp_en",
             #"L226.SubsectorInterpTo_en",
             "L226.GlobalTechCost_en",
             "L226.GlobalTechShrwt_en",
             "L226.StubTechCoef_electd"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L226.DeleteSupplysector_CHINAelec",
             "L226.StubTechCoef_electd_CHINA",
             "L226.TechShrwt_electd_CHINA",
             "L226.TechCost_electd_CHINA",
             "L226.Supplysector_electd_CHINA",
             "L226.SubsectorShrwtFllt_electd_CHINA",
             "L226.SubsectorLogit_electd_CHINA",
             "L226.SubsectorInterp_electd_CHINA",
             "L226.TechCoef_electd_CHINA",
             "L226.TechShrwt_en_CHINA",
             "L226.TechCoef_en_CHINA",
             "L226.TechCost_en_CHINA",
             "L226.Supplysector_en_CHINA",
             "L226.SubsectorShrwtFllt_en_CHINA",
             "L226.SubsectorLogit_en_CHINA",
             "L226.Ccoef_CHINA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = T)
    A21.sector <- get_data(all_data, "energy/A21.sector", strip_attributes = T)
    A26.sector <- get_data(all_data, "energy/A26.sector", strip_attributes = T)
    regional_fuel_prices_RMB <- get_data(all_data, "gcam-china/regional_fuel_prices_RMB", strip_attributes = T)
    L202.CarbonCoef <- get_data(all_data, "L202.CarbonCoef", strip_attributes = T)
    L101.NBS_use_all_Mtce <- get_data(all_data, "L101.NBS_use_all_Mtce", strip_attributes = T)
    L226.Supplysector_en <- get_data(all_data, "L226.Supplysector_en", strip_attributes = T)
    L226.SubsectorLogit_en <- get_data(all_data, "L226.SubsectorLogit_en", strip_attributes = T)
    #L226.SubsectorShrwt_en <- get_data(all_data, "L226.SubsectorShrwt_en")
    L226.SubsectorShrwtFllt_en <- get_data(all_data, "L226.SubsectorShrwtFllt_en", strip_attributes = T)
    L226.SubsectorInterp_en <- get_data(all_data, "L226.SubsectorInterp_en", strip_attributes = T)
    #L226.SubsectorInterpTo_en <- get_data(all_data, "L226.SubsectorInterpTo_en")
    L226.GlobalTechCost_en <- get_data(all_data, "L226.GlobalTechCost_en", strip_attributes = T)
    L226.GlobalTechShrwt_en <- get_data(all_data, "L226.GlobalTechShrwt_en", strip_attributes = T)
    L226.StubTechCoef_electd <- get_data(all_data, "L226.StubTechCoef_electd", strip_attributes = T)

    # silence check package notes
    region <- supplysector <- from.year <- to.year <- output.unit <- input.unit <- price.unit <- liq_adj <-
      logit.exponent <- logit.type <- . <- subsector <- State <- Coal <- Natural.gas <- Distillate.fuel.oil <-
      grid.region <- state_name <- coal_adj <- gas_adj <- liq_adju <- sector1 <- adjustment <- technology <-
      year <- minicam.non.energy.input <- tmp <- sector2 <- trash1 <- trash2 <- input.cost <- sector.name <-
      subsector.name <- stub.technology <- market.name <- state <- coal <- gas <- refined.liquids <- EBMaterial <-
      value <- province <- coal_weight <- gas_weight <- liq_weight <- adj_price <- NULL

    # global_energy_to_CHINA_electd - takes global energy inputs from L226.en_distribution.R
    # and processes for use in CHINA electricity T&D
    global_energy_to_CHINA_electd <- function(data) {
      data %>%
        filter(region == gcamchina.REGION,
               supplysector %in% gcamchina.ELECT_TD_SECTORS) %>%
        write_to_all_provinces(names(data), gcamchina.PROVINCES_ALL)
    } # end global_energy_to_CHINA_electd

    # Process inputs:
    L226.SubsectorInterp_en %>%
      mutate(from.year = as.integer(from.year),
             to.year = as.integer(to.year)) ->
      L226.SubsectorInterp_en

    # Build tables

    # Supplysector information

    # PART 1: FUEL HANDLING AND DELIVERY SECTORS

    # L226.Supplysector_en_CHINA: Supply sector information for energy handling and delivery sectors
    A21.sector %>%
      select(supplysector, output.unit, input.unit, price.unit, logit.exponent, logit.type) %>%
      filter(supplysector %in% gcamchina.REGIONAL_FUEL_MARKETS) ->
      A21.tmp

    A26.sector  %>%
      select(supplysector, output.unit, input.unit, price.unit, logit.exponent, logit.type) %>%
      filter(supplysector %in% gcamchina.REGIONAL_FUEL_MARKETS) %>%
      bind_rows(A21.tmp) %>%
      repeat_add_columns(tibble(region = unique(province_names_mappings$province))) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS)) ->
      L226.Supplysector_en_CHINA


    # L226.SubsectorShrwtFllt_en_CHINA: subsector shareweights of energy handling and delivery
    L226.Supplysector_en_CHINA %>%
      mutate(subsector = supplysector,
             year.fillout = min(MODEL_BASE_YEARS),
             share.weight = gcamchina.DEFAULT_SHAREWEIGHT) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]]) ->
      L226.SubsectorShrwtFllt_en_CHINA


    # L226.SubsectorLogit_en_CHINA
    L226.SubsectorShrwtFllt_en_CHINA %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = gcamchina.DEFAULT_LOGITEXP,
             logit.type = gcamchina.DEFAULT_LOGIT_TYPE) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME) ->
      L226.SubsectorLogit_en_CHINA


    # L226.TechShrwt_en_CHINA: technology shareweights of energy handling and delivery
    L226.SubsectorShrwtFllt_en_CHINA %>%
      select(LEVEL2_DATA_NAMES[["Subsector"]]) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subsector,
             share.weight = gcamchina.DEFAULT_SHAREWEIGHT) %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]], "share.weight") ->
      L226.TechShrwt_en_CHINA


    # L226.TechCoef_en_CHINA: technology coefficients and market names of energy handling and delivery
    L226.TechShrwt_en_CHINA %>%
      select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
      mutate(minicam.energy.input = supplysector,
             coefficient = gcamchina.DEFAULT_COEFFICIENT,
             market.name = gcamchina.DEFAULT_MARKET) ->
      L226.TechCoef_en_CHINA


    # L226.CostAdj_75USDGJ_FERC_F: grid region specific cost adders
    # NOTE: the average national costs are already accounted in the corresponding sectors of the CHINA;
    # this table implements a price adjustment factor
    #
    # Step 1 Convert the units to 1975 USD per GJ
    #Match in region acronyms
    regional_fuel_prices_RMB %>%
      left_join_error_no_match(province_names_mappings, by = "province.name") %>%
    #Unit conversions
      mutate(coal = coal * gcamchina.CONV_2015_2010_RMB * gcamchina.CONV_2010_RMB_USD * gdp_deflator(1975, 2010) * CONV_EJ_MTCE / 1000,
             gas =  gas * gcamchina.CONV_2015_2010_RMB * gcamchina.CONV_2010_RMB_USD * gdp_deflator(1975, 2010) / CONV_BCM_EJ / 1000,
             refined.liquids = refined.liquids * gcamchina.CONV_2014_2010_RMB * gcamchina.CONV_2010_RMB_USD * gdp_deflator(1975, 2010) / CONV_TONNE_GJ_DIESEL) ->
      regional_fuel_prices

    # Step 2 Consumption weights for calculating average national resource prices
    L101.NBS_use_all_Mtce %>%
      filter(year == 2010, EBMaterial == "Diesel Oil") %>%
      mutate(value = if_else(is.na(value),0,value)) %>%
      group_by(province) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      rename(liq_weight = value) ->
      liquids_use_weights

    L101.NBS_use_all_Mtce %>%
      filter(year == 2010, EBMaterial %in% c("LNG","Natural Gas")) %>%
      mutate(value = if_else(is.na(value),0,value)) %>%
      group_by(province) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      rename(gas_weight = value) ->
      gas_use_weights

    L101.NBS_use_all_Mtce %>%
      filter(year == 2010, EBMaterial == "Coal Raw") %>%
      mutate(value = if_else(is.na(value),0,value))  %>%
      group_by(province) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      rename(coal_weight = value) ->
      coal_use_weights

    regional_fuel_prices %>%
      left_join_error_no_match(liquids_use_weights, by = c("province")) %>%
      left_join_error_no_match(gas_use_weights, by = c("province")) %>%
      left_join_error_no_match(coal_use_weights, by = c("province")) %>%
      # Step 3 Final calculation of cost adjustment
      mutate(coal_adj = coal - weighted.mean(coal, coal_weight),
             gas_adj = gas - weighted.mean(gas, gas_weight),
             liq_adj = refined.liquids - weighted.mean(refined.liquids, liq_weight)) %>%
      select(region = province, coal, refined.liquids, gas, coal_weight, liq_weight, gas_weight, coal_adj, liq_adj, gas_adj) ->
      regional_fuel_prices

    regional_fuel_prices_long <- regional_fuel_prices %>%
      select(region, coal_adj, liq_adj, gas_adj) %>%
      gather(key = "adj_price", value = "value", c("coal_adj", "liq_adj", "gas_adj")) %>%
      mutate(adj_price = if_else(adj_price == "coal_adj", "coal",
                                 if_else(adj_price == "liq_adj", "liquids",
                                         "gas")))


    #Leave this here just in case of future aggregation to grid region
    # regional_fuel_prices %>%
    #   group_by(grid.region) %>%
    #   summarize(coal_adj = median(coal_adj),
    #             gas_adj = median(gas_adj),
    #             liq_adj = median(liq_adj)) %>%
    #   ungroup %>%
    #   na.omit ->
    #   L226.CostAdj_75USDGJ_FERC_F


    # L226.TechCost_en_CHINA: cost adders
      L226.TechShrwt_en_CHINA %>%
        select(LEVEL2_DATA_NAMES[["TechYr"]]) %>%
        mutate(minicam.non.energy.input = "regional price adjustment",
               adj_price = if_else(grepl("coal", supplysector), "coal",
                                   if_else(grepl("liquid", supplysector), "liquids",
                                           "gas"))) %>%
        # must use left join, HK and MC have NA values
        left_join(regional_fuel_prices_long, by = c("region", "adj_price")) %>%
        mutate(input.cost = round(value, energy.DIGITS_COST)) %>%
        select(-adj_price, value) ->
        L226.TechCost_en_CHINA

    # L226.Ccoef_CHINA: carbon coef for cost adder sectors
    L202.CarbonCoef %>%
      filter(region == gcamchina.REGION) %>%
      select(-region) ->
      L226.Ccoef_CHINA

    L226.TechShrwt_en_CHINA %>%
      select(region, supplysector) %>%
      distinct %>%
      left_join_error_no_match(L226.Ccoef_CHINA, by = c("supplysector" = "PrimaryFuelCO2Coef.name")) ->
      L226.Ccoef_CHINA



    # PART 2: ELECTRICITY TRANSMISSION AND DISTRIBUTION

    # L226.DeleteSupplysector_CHINAelec: Removing the electricity T&D sectors of the CHINA region
    # This should probably be converted to an assumption and read in at some point.
    L226.DeleteSupplysector_CHINAelec <- tibble(supplysector = gcamchina.ELECT_TD_SECTORS) %>%
      mutate(region = gcamchina.REGION)

    # Replacing for loop starting on line 161 in old DS.
    # There's also two inputs to this chunk that are NULL, and nothing gets done to: L226.SubsectorShrwt_en, L226.SubsectorInterpTo_en
    L226.Supplysector_en %>%
      global_energy_to_CHINA_electd() ->
      L226.Supplysector_electd_CHINA

    L226.SubsectorLogit_en %>%
      global_energy_to_CHINA_electd() ->
      L226.SubsectorLogit_electd_CHINA

    L226.SubsectorShrwtFllt_en %>%
      global_energy_to_CHINA_electd() ->
      L226.SubsectorShrwtFllt_electd_CHINA

    L226.SubsectorInterp_en %>%
      global_energy_to_CHINA_electd() ->
      L226.SubsectorInterp_electd_CHINA


    # Using national electric markets
    if(!gcamchina.USE_REGIONAL_ELEC_MARKETS) {

      # L226.StubTechCoef_electd_CHINA: Using national elec markets. State elect_td sectors are treated as stub technologies
      L226.StubTechCoef_electd %>%
        global_energy_to_CHINA_electd() ->
        L226.StubTechCoef_electd_CHINA

    }


    # Using regional electric markets
    if(gcamchina.USE_REGIONAL_ELEC_MARKETS) {

      # The elect_td sectors can not use the global tech database as their input is different.

      # L226.TechShrwt_electd_CHINA: Tech share weights for electricity T&D
      L226.GlobalTechShrwt_en %>%
        filter(sector.name %in% gcamchina.ELECT_TD_SECTORS) %>%
        write_to_all_provinces(c("region", names(L226.GlobalTechShrwt_en)), gcamchina.PROVINCES_ALL) %>%
        rename(supplysector = sector.name,
               subsector = subsector.name) ->
        L226.TechShrwt_electd_CHINA

      # L226.TechCost_electd_CHINA: Tech costs for electricity T&D
      L226.GlobalTechCost_en %>%
        filter(sector.name %in% gcamchina.ELECT_TD_SECTORS) %>%
        write_to_all_provinces(c("region", names(L226.GlobalTechCost_en)), gcamchina.PROVINCES_ALL) %>%
        rename(supplysector = sector.name,
               subsector = subsector.name) ->
        L226.TechCost_electd_CHINA

      # L226.TechCoef_electd_CHINA: Tech coefficients for electricity T&D
      L226.StubTechCoef_electd %>%
        global_energy_to_CHINA_electd() %>%
        rename(technology = stub.technology) %>%
        mutate(minicam.energy.input = "electricity domestic supply") %>%
        select(-market.name) %>%
        left_join_error_no_match(select(province_names_mappings, grid.region, province), by = c("region" = "province")) %>%
        rename(market.name = grid.region) ->
        L226.TechCoef_electd_CHINA
    }


    # Produce outputs
    L226.DeleteSupplysector_CHINAelec %>%
      add_title("Removing the electricity T&D sectors of the CHINA region") %>%
      add_units("NA") %>%
      add_comments("Removing the electricity T&D sectors of the CHINA region") %>%
      add_legacy_name("L226.DeleteSupplysector_CHINAelec") ->
      L226.DeleteSupplysector_CHINAelec

    missing_data() %>%
      add_legacy_name("L226.StubTechCoef_electd_CHINA") %>%
      add_precursors("L226.StubTechCoef_electd") ->
      L226.StubTechCoef_electd_CHINA

    L226.TechShrwt_electd_CHINA %>%
      add_title("Tech share weights for elec T&D when using regional electricity markets") %>%
      add_units("NA") %>%
      add_comments("Tech share weights for elec T&D when using regional electricity markets") %>%
      add_comments("The elect_td sectors can not use the global tech database as their input is different.") %>%
      add_legacy_name("L226.TechShrwt_electd_CHINA") %>%
      add_precursors("L226.GlobalTechShrwt_en") ->
      L226.TechShrwt_electd_CHINA

    L226.TechCost_electd_CHINA %>%
      add_title("Tech costs for elec T&D when using regional electricity markets") %>%
      add_units("1975$") %>%
      add_comments("Tech costs for elec T&D when using regional electricity markets") %>%
      add_comments("The elect_td sectors can not use the global tech database as their input is different.") %>%
      add_legacy_name("L226.TechCost_electd_CHINA") %>%
      add_precursors("L226.GlobalTechCost_en") ->
      L226.TechCost_electd_CHINA

    L226.TechCoef_electd_CHINA %>%
      add_title("Tech coefficients for elec T&D when using regional electricity markets") %>%
      add_units("NA") %>%
      add_comments("Tech coeff for elec T&D when using regional electricity markets.") %>%
      add_comments("The elect_td sectors can not use the global tech database as their input is different.") %>%
      add_legacy_name("L226.TechCoef_electd_CHINA") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "L226.StubTechCoef_electd") ->
      L226.TechCoef_electd_CHINA

    L226.Supplysector_en_CHINA %>%
      add_title("Supply sector information for energy handling and delivery sectors.") %>%
      add_units("varies") %>%
      add_comments("Supply sector information for energy handling and delivery sectors for CHINA grid regions.") %>%
      add_comments("Currently using FERC regions as a proxy for regional energy markets.") %>%
      add_legacy_name("L226.Supplysector_en_CHINA") %>%
      add_precursors("energy/A21.sector",
                     "energy/A26.sector") ->
      L226.Supplysector_en_CHINA

    L226.SubsectorShrwtFllt_en_CHINA %>%
      add_title("Subsector shareweights of energy handling and delivery") %>%
      add_units("NA") %>%
      add_comments("Subsector shareweights of energy handling and delivery") %>%
      add_legacy_name("L226.SubsectorShrwtFllt_en_CHINA") %>%
      same_precursors_as(L226.Supplysector_en_CHINA) ->
      L226.SubsectorShrwtFllt_en_CHINA

    L226.SubsectorLogit_en_CHINA %>%
      add_title("Logit info for energy subsectors") %>%
      add_units("NA") %>%
      add_comments("Logit info for energy subsectors.") %>%
      add_comments("There is only one tech per subsector so the logit choice does not matter.") %>%
      add_legacy_name("L226.SubsectorLogit_en_CHINA") %>%
      same_precursors_as(L226.SubsectorShrwtFllt_en_CHINA) ->
      L226.SubsectorLogit_en_CHINA

    L226.TechShrwt_en_CHINA %>%
      add_title("Technology shareweights of energy handling and delivery") %>%
      add_units("NA") %>%
      add_comments("Technology shareweights of energy handling and delivery.") %>%
      add_comments("Can't use stub technologies because these would inherit the wrong energy-inputs.") %>%
      add_legacy_name("L226.TechShrwt_en_CHINA") %>%
      same_precursors_as(L226.SubsectorShrwtFllt_en_CHINA) ->
      L226.TechShrwt_en_CHINA

    L226.TechCoef_en_CHINA %>%
      add_title("Technology coefficients and market names of energy handling and delivery") %>%
      add_units("units") %>%
      add_comments("Technology coefficients and market names of energy handling and delivery") %>%
      add_legacy_name("L226.TechCoef_en_CHINA") %>%
      same_precursors_as(L226.TechShrwt_en_CHINA) ->
      L226.TechCoef_en_CHINA

    if(exists("L226.TechCost_en_CHINA")) {
      L226.TechCost_en_CHINA %>%
        add_title("Regional price adjustments/cost adders for CHINA energy.") %>%
        add_units("1975$/GJ") %>%
        add_comments("Regional price adjustments/cost adders for CHINA energy") %>%
        add_legacy_name("L226.TechCost_en_CHINA") %>%
        add_precursors("gcam-china/province_names_mappings",
                       "energy/A21.sector",
                       "energy/A26.sector",
                       "gcam-china/regional_fuel_prices_RMB",
                       "L101.NBS_use_all_Mtce") ->
        L226.TechCost_en_CHINA
    } else {
      # If gcamchina.USE_REGIONAL_FUEL_MARKETS is FALSE,
      # indicating not to use regional "cost adders to
      # differentiate fuel prices by grid region in GCAM-CHINA,
      # then blank tibbles of the "cost adders" are produced.
      missing_data() %>%
        add_legacy_name("L226.TechCost_en_CHINA") %>%
        add_precursors("gcam-china/province_names_mappings",
                       "energy/A21.sector",
                       "energy/A26.sector",
                       "gcam-china/regional_fuel_prices_RMB",
                       "L101.NBS_use_all_Mtce") ->
        L226.TechCost_en_CHINA
    }

    L226.Ccoef_CHINA %>%
      add_title("Carbon coef for cost adder sectors") %>%
      add_units("NA") %>%
      add_comments("Carbon coef for cost adder sectors") %>%
      add_legacy_name("L226.Ccoef") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "energy/A21.sector",
                     "energy/A26.sector",
                     "gcam-china/regional_fuel_prices_RMB",
                     "L202.CarbonCoef") ->
      L226.Ccoef_CHINA

    L226.Supplysector_electd_CHINA %>%
      add_title("CHINA supply sector input, output, and logit info for elec T&D") %>%
      add_units("varies") %>%
      add_comments("CHINA supply sector input, output, and logit info for elec T&D by province") %>%
      add_legacy_name("L226.Supplysector_electd_CHINA") %>%
      add_precursors("L226.Supplysector_en") ->
      L226.Supplysector_electd_CHINA

    L226.SubsectorLogit_electd_CHINA %>%
      add_title("CHINA subsector logit info for elec T&D") %>%
      add_units("varies") %>%
      add_comments("CHINA subsector logit info for elec T&D by grid.region") %>%
      add_legacy_name("L226.SubsectorLogit_electd_CHINA") %>%
      add_precursors("L226.SubsectorLogit_en") ->
      L226.SubsectorLogit_electd_CHINA

    L226.SubsectorShrwtFllt_electd_CHINA %>%
      add_title("CHINA subsector shareweight fillout for elec T&D") %>%
      add_units("varies") %>%
      add_comments("CHINA subsector shareweight fillout for elec T&D by province") %>%
      add_legacy_name("L226.SubsectorShrwtFllt_electd_CHINA") %>%
      add_precursors("L226.SubsectorShrwtFllt_en") ->
      L226.SubsectorShrwtFllt_electd_CHINA

    L226.SubsectorInterp_electd_CHINA %>%
      add_title("CHINA interpolation info for elec T&D") %>%
      add_units("varies") %>%
      add_comments("CHINA interpolation info for elec T&D by province") %>%
      add_legacy_name("L226.SubsectorInterp_electd_CHINA") %>%
      add_precursors("L226.SubsectorInterp_en") ->
      L226.SubsectorInterp_electd_CHINA

    return_data(L226.DeleteSupplysector_CHINAelec,
                L226.StubTechCoef_electd_CHINA,
                L226.TechShrwt_electd_CHINA,
                L226.TechCost_electd_CHINA,
                L226.TechCoef_electd_CHINA,
                L226.Supplysector_en_CHINA,
                L226.SubsectorShrwtFllt_en_CHINA,
                L226.SubsectorLogit_en_CHINA,
                L226.TechShrwt_en_CHINA,
                L226.TechCoef_en_CHINA,
                L226.TechCost_en_CHINA,
                L226.Ccoef_CHINA,
                L226.Supplysector_electd_CHINA,
                L226.SubsectorLogit_electd_CHINA,
                L226.SubsectorShrwtFllt_electd_CHINA,
                L226.SubsectorInterp_electd_CHINA)
  } else {
    stop("Unknown command")
  }
}
