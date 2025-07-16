# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L203.water_td_CHINA
#'
#' Mapping of water consumption/withdrawal to sectoral demands at the province level.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L203.DeleteSupplysector_CHINA}, \code{L203.Supplysector_CHINA}, \code{L203.SubsectorLogit_CHINA},
#' \code{L203.SubsectorShrwt_CHINA}, \code{L203.TechShrwt_CHINA}, \code{L203.TechCoef_CHINA}, \code{L203.TechPmult_CHINA},
#' \code{L203.TechDesalCoef_CHINA}, \code{L203.TechDesalShrwt_CHINA}, \code{L203.TechDesalCost_CHINA}.
#' The corresponding file in the original data system was \code{L203.water.mapping.R} (water level2).
#' @details Generates water mapping sector input files to group demands by sectors.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author NTG May 2020
module_gcamchina_L203.water_td_CHINA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "water/basin_to_country_mapping",
             FILE = "water/water_td_sectors",
             FILE = "water/A71.sector",
             FILE = "water/A72.sector",
             FILE = "water/A73.sector",
             FILE = "water/A74.sector",
             "L103.water_mapping_CHINA_R_LS_W_Ws_share",
             "L103.water_mapping_CHINA_R_PRI_W_Ws_share",
             "L103.water_mapping_CHINA_R_GLU_W_Ws_share",
             "L103.water_mapping_CHINA_R_B_W_Ws_share",
             FILE = "gcam-china/provinces_subregions",
             FILE = "gcam-china/province_and_basin",
             FILE = "gcam-china/china_seawater_provinces_basins",
             FILE = "water/water_td_sectors",
             FILE = "water/A03.sector",
             "L201.RsrcTechCoef",
             "L203.Supplysector_desal_basin"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L203.DeleteSupplysector_CHINA",
             "L203.DeleteResTechInput_CHINA",
             "L203.DeleteSubsector_CHINA",
             "L203.Supplysector_CHINA",
             "L203.SubsectorLogit_CHINA",
             "L203.SubsectorShrwt_CHINA",
             "L203.TechShrwt_CHINA",
             "L203.TechCoef_CHINA",
             "L203.TechPmult_CHINA",
             "L203.TechDesalCoef_CHINA",
             "L203.TechDesalShrwt_CHINA",
             "L203.TechDesalCost_CHINA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    water_td_sectors <- get_data(all_data, "water/water_td_sectors")
    A71.sector <- get_data(all_data, "water/A71.sector")
    A72.sector <- get_data(all_data, "water/A72.sector")
    A73.sector <- get_data(all_data, "water/A73.sector")
    A74.sector <- get_data(all_data, "water/A74.sector")
    L103.water_mapping_CHINA_R_LS_W_Ws_share <- get_data(all_data, "L103.water_mapping_CHINA_R_LS_W_Ws_share", strip_attributes = TRUE)
    L103.water_mapping_CHINA_R_PRI_W_Ws_share <- get_data(all_data, "L103.water_mapping_CHINA_R_PRI_W_Ws_share", strip_attributes = TRUE)
    L103.water_mapping_CHINA_R_GLU_W_Ws_share <- get_data(all_data,"L103.water_mapping_CHINA_R_GLU_W_Ws_share", strip_attributes = TRUE)
    L103.water_mapping_CHINA_R_B_W_Ws_share <- get_data(all_data,"L103.water_mapping_CHINA_R_B_W_Ws_share", strip_attributes = TRUE)
    GCAM_province_names <- get_data(all_data, "gcam-china/provinces_subregions")
    province_and_basin <- get_data(all_data, "gcam-china/province_and_basin")
    china_seawater_provinces_basins <- get_data(all_data, "gcam-china/china_seawater_provinces_basins")
    water_td_sectors <- get_data(all_data, "water/water_td_sectors")
    A03.sector <- get_data(all_data, "water/A03.sector")
    L201.RsrcTechCoef <- get_data(all_data, "L201.RsrcTechCoef", strip_attributes = TRUE)
    L203.Supplysector_desal_basin <- get_data(all_data, "L203.Supplysector_desal_basin", strip_attributes = TRUE)

    GLU <- GLU_code <- GLU_name <- water.sector <-
      water_type <- supplysector <- field.eff <- conveyance.eff <-
      coefficient <- region <- state <- share <- basin_name <- Basin_name <-
      GCAM_basin_ID <- state_abbr <- water_sector <- year <- wt_short <- value <-
      state.to.country.share <- subsector <- technology <- share.weight <-
      price.unit <- input.unit <- output.unit <- logit.exponent <- logit.type <-
      logit.year.fillout <- resource <- minicam.energy.input <- subresource <- NULL  # silence package check notes

    # Define unique provinces and basins that have access to seawater that will
    # allow for seawater cooling

    seawater_provinces_basins <- unique(china_seawater_provinces_basins$seawater_region)

    # Define in which province GCAM water basins exist by using data from R package created by Chris Vernon
    province_and_basin %>%
      left_join_error_no_match(basin_to_country_mapping, by = "GCAM_basin_ID") %>%
      select(GCAM_basin_ID, GLU_name, basin_name, state_abbr) %>%
      rename(region = state_abbr) ->
      province_and_basin_mapping

    # Create mappings for the sectors that have production at the province level already.
    # These sectors: Industrial, Municipal, and Electricity will not need to be shared
    # from the china region to the provinces, and thus will not have separate market names by region
    L103.water_mapping_CHINA_R_B_W_Ws_share %>%
      mutate(water_sector = gsub("Domestic", "Municipal", water_sector)) %>%
      left_join_error_no_match(water_td_sectors, by = c("water_sector" = "water.sector")) %>%
      left_join_error_no_match(A03.sector, by = "supplysector", ignore_columns = c("logit.type")) %>%
      mutate(supplysector = set_water_input_name(water_sector, water_type, water_td_sectors)) ->
      L203.mapping_nonirr

    # Using irrigation shares, define water sector and add demand categories
    L103.water_mapping_CHINA_R_GLU_W_Ws_share %>%
      rename(state = region) %>%
      mutate(region = gcamchina.REGION,
             water.sector = water.IRRIGATION) %>%
      left_join_error_no_match(water_td_sectors, by = "water.sector") %>%
      left_join_error_no_match(A03.sector, by = "supplysector", ignore_columns = "logit.type") %>%
      mutate(supplysector = set_water_input_name(water.sector, water_type, water_td_sectors, GLU_name)) ->
      L203.mapping_irr

    # Isolate the China region which will share basin level demands in the China region to
    # provinces which are defined as subsectors
    L203.mapping_irr %>%
      mutate(subsector = state,
             technology = supplysector,
             coefficient = if_else(water.sector == water.IRRIGATION & water_type == "water withdrawals",
                                   1 / 1, 1),
             ## ^^ conveyance losses for irrigation--applied to withdrawals only
             # Note: Conveyance losses are taken out of agriculture withdrawals and...
             # ... instead applied to water distribution sectors (water_td_irr). This means that to get total...
             # ... ag withdrawals for reporting (i.e., when querying GCAM results)...
             # ... it is necessary to include the conveyance loss.
             share.weight = share,
             market.name = state,
             share.weight.year = year,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      select(-share, -state) %>%
      arrange(region) ->
      L203.mapping_irr_region

    # We must now set all subsectors in China from gcam-core and water_mapping.xml to 0 so that we do not double count
    # demands
    L203.mapping_irr_region %>%
      bind_rows(L203.mapping_irr_region %>%
                  mutate(subsector = basin_name,
                         technology = basin_name,
                         share.weight = 0,
                         market.name = gcamchina.REGION)) ->
      L203.mapping_irr_region

    # Isolate the provinces and define the basins which contribute water supplies to wach one.
    L203.mapping_irr %>%
      select(-region) %>%
      mutate(region = state,
             subsector = basin_name,
             technology = basin_name,
             coefficient = gcamchina.DEFAULT_COEFFICIENT,
             share.weight = gcamchina.DEFAULT_SHAREWEIGHT,
             market.name = gcamchina.REGION,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      select(-share, -state) %>%
      arrange(region) ->
      L203.mapping_irr_province

    # Combine province and China region irrigation mappings
    bind_rows(
      L203.mapping_irr_region %>%
        ## filter out basin name subsectors
        filter(subsector %in% gcamchina.PROVINCES_NOHKMC),
      L203.mapping_irr_province
    ) ->
      L203.mapping_irr


    # Livestock sector:
    # This done slightly different as production of livestock is not modeled at the province level.
    # Here we take the regional (i.e. China) water demands of livestock and map them to the provincial level based on
    # the amount of water for livestock that each province requires compared to the china as a whole, computed in
    # L103.water_mapping_USA
    L103.water_mapping_CHINA_R_LS_W_Ws_share %>%
      mutate(region=gcamchina.REGION,
             water.sector = water.LIVESTOCK) %>%
      left_join_error_no_match(water_td_sectors, by = "water.sector") %>%
      left_join_error_no_match(A03.sector, by = "supplysector", ignore_columns = "logit.type") %>%
      mutate(wt_short = water.MAPPED_WATER_TYPES_SHORT[water_type],
             supplysector = paste(supplysector, wt_short, sep = "_"),
             coefficient = gcamchina.DEFAULT_COEFFICIENT,
             subsector = state,
             technology = supplysector,
             share.weight = value,
             market.name = state,
             share.weight.year = year,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      select(-wt_short, -value, -state) %>%
      arrange(region) ->
      L203.mapping_livestock

    L203.mapping_livestock %>%
      bind_rows(L203.mapping_livestock %>%
                  # LJENM returns error because number of rows in data changes.
                  # The join is intended to duplicate rows because some states
                  # are mapped to multiple basisn.  Thus, left_join() is used.
                  left_join(province_and_basin_mapping, by = c("subsector" = "region")) %>%
                  mutate(share.weight = 0,
                         subsector = basin_name,
                         technology = basin_name,
                         market.name = gcamchina.REGION) %>%
                  unique()
      ) ->
      L203.mapping_livestock

    # (d) primary energy sector
    # We use USGS withdrawal data for primary energy mining and ratios of fresh to saline water withdrawals to
    # map the demands from USA values to state level. This is done in 2 parts in order to specify differences in
    # subsectors at the state and national levels, as well as differences in share weights (i.e. mapping to states,
    # mapping of fresh to desal within a state)

    L103.water_mapping_CHINA_R_PRI_W_Ws_share %>%
      mutate(region = gcamchina.REGION,
             water.sector = water.PRIMARY_ENERGY) %>%
      left_join_error_no_match(water_td_sectors, by = "water.sector") %>%
      left_join_error_no_match(A03.sector, by = "supplysector", ignore_columns = "logit.type") %>%
      mutate(wt_short = water.MAPPED_WATER_TYPES_SHORT[water_type],
             supplysector = paste(supplysector, wt_short, sep = "_"),
             coefficient = gcamchina.DEFAULT_COEFFICIENT,
             subsector = state,
             technology = supplysector,
             share.weight = state.to.country.share,
             market.name = state,
             share.weight.year = year,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      select(-wt_short, -state.to.country.share, -state) %>%
      arrange(region) ->
      L203.mapping_primary_region

    L203.mapping_primary_region %>%
      bind_rows(L203.mapping_primary_region %>%
                  # LJENM returns error because number of rows in data changes.
                  # The join is intended to duplicate rows because some states
                  # are mapped to multiple basisn.  Thus, left_join() is used.
                  left_join(province_and_basin_mapping, by = c("subsector" = "region")) %>%
                  mutate(share.weight = 0,
                         subsector = basin_name,
                         technology = basin_name,
                         market.name = gcamchina.REGION) %>%
                  unique()
      ) ->
      L203.mapping_primary_region

    # No values are present for DC, therefore NAs are created. These are replaced with
    # zero shareweights

    L203.mapping_primary_region %>%
      replace_na(list(share.weight = 0)) %>%
      replace_na(list(fresh.share = 0)) ->
      L203.mapping_primary


    # combine all sectors and add additional required columns. Long format is used for
    # subsector share weights, additional mapping is used for all other final outputs

    L203.mapping_nonirr %>%
      mutate(coefficient = gcamchina.DEFAULT_COEFFICIENT,
             subsector = basin_name,
             technology = basin_name,
             share.weight = share,
             logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
      arrange(region) %>%
      bind_rows(L203.mapping_nonirr %>%
                  filter(year == gcamusa.FINAL_MAPPING_YEAR) %>%
                  mutate(year=max(MODEL_BASE_YEARS),
                         coefficient = gcamchina.DEFAULT_COEFFICIENT,
                         subsector = basin_name,
                         technology = basin_name,
                         share.weight = share,
                         logit.year.fillout = first(MODEL_BASE_YEARS)) %>%
                  arrange(region)) %>%
      complete(nesting(region, supplysector, subsector, technology, water.sector, basin_name, water_type, coefficient, share,
                       share.weight, price.unit, input.unit, output.unit, logit.exponent, logit.type, logit.year.fillout),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      dplyr::filter(!is.na(year)) %>%
      bind_rows(L203.mapping_livestock %>%
                  ## Filter out basin names in subsectors as these are deleted later
                  filter(subsector %in% gcamchina.PROVINCES_NOHKMC),
                L203.mapping_primary %>%
                  filter(subsector %in% gcamchina.PROVINCES_NOHKMC),
                L203.mapping_irr) %>%
      mutate(pMult = if_else(water.sector == water.IRRIGATION & water_type == "water withdrawals" & region != gcamchina.REGION,
                             water.IRR_PRICE_SUBSIDY_MULT, water.MAPPING_PMULT)) ->
      L203.mapping_all

    L203.EFW_delete_supplysectors <- bind_rows(A71.sector, A72.sector, A73.sector, A74.sector) %>%
      pull(supplysector)
    L203.delete_desal_basin_sectors <- L203.Supplysector_desal_basin %>%
      filter(region == gcamchina.REGION) %>%
      pull(supplysector)
    tibble(region = gcamchina.REGION,
           supplysector = c(water.DELETE_DEMAND_TYPES,
                            L203.EFW_delete_supplysectors,
                            L203.delete_desal_basin_sectors)) ->
      L203.DeleteSupplysector_CHINA

    ## Also need to delete the "elect_td_ind" input to the groundwater grades in future periods
    L201.RsrcTechCoef %>%
      filter(region == gcamchina.REGION) %>%
      select(region, resource, subresource, technology, year, minicam.energy.input) ->
      L203.DeleteResTechInput_CHINA

    ## We delete the basin level subsectors in the China region
    ## to eliminate double counting of irrigation, livestock,
    ## and primary energy. This overrides the mappings from
    ## water_mapping.XML and maps directly to the provinces.
    L203.mapping_irr_region %>%
      filter(!subsector %in% gcamchina.PROVINCES_NOHKMC) %>%
      select(region,supplysector,subsector) %>%
      bind_rows(
        L203.mapping_primary_region %>%
          filter(!subsector %in% gcamchina.PROVINCES_NOHKMC) %>%
          select(region,supplysector,subsector),
        L203.mapping_livestock%>%
          filter(!subsector %in% gcamchina.PROVINCES_NOHKMC) %>%
          select(region,supplysector,subsector)
      ) %>%
      unique()->
      L203.DeleteSubsector_CHINA


    # Sector information
    L203.mapping_all %>%
      select(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME) ->
      L203.Supplysector_CHINA

    # Subsector logit exponents for mapping sector
    L203.mapping_all %>%
      mutate(logit.exponent = if_else(region != gcamchina.REGION, water.LOGIT_EXP, 0)) %>%
      select(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME) ->
      L203.SubsectorLogit_CHINA

    # Subsector share weights to 1 (no competition) in all provinces. Sharing happens at China level. Water prices
    # will drive competition between the basins at the province level
    L203.mapping_all %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwt"]]) ->
      L203.SubsectorShrwt_CHINA

    # Technology share weights, defined by province and sector
    # Zero out technology shareweights in the China region to make sure values are not counted multiple times
    L203.mapping_all %>%
      complete(nesting(region, supplysector, subsector, technology, water.sector, basin_name, water_type, coefficient),
               year = c(year, MODEL_BASE_YEARS,MODEL_FUTURE_YEARS)) %>%
      mutate(share.weight = if_else(region == gcamchina.REGION & !(subsector %in% gcamchina.PROVINCES_NOHKMC) & !grepl("irr", supplysector), 0, 1)) %>%
      dplyr::filter(!is.na(year)) %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]]) ->
      L203.TechShrwt_CHINA

    # Define market name and minicam energy input dependent upon whether the sector is
    # produced at the province level or is we map from China region to province
    L203.mapping_all %>%
      complete(nesting(region, supplysector, subsector, technology, water.sector, basin_name, water_type, coefficient),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      mutate(minicam.energy.input = if_else((region == gcamchina.REGION & grepl("water_td", technology)),
                                            supplysector,
                                            paste0(basin_name, "_", water_type)),
             market.name = if_else((region == gcamchina.REGION & grepl("water_td", technology)), subsector, gcamchina.REGION)) %>%
      dplyr::filter(!is.na(year)) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]]) ->
      L203.TechCoef_CHINA

    # Pass-through technology water price adjust if there one
    L203.mapping_all %>%
      complete(nesting(region, supplysector, subsector, technology, water.sector, basin_name, water_type, coefficient),
               year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      replace_na(list(pMult=1)) %>%
      select(LEVEL2_DATA_NAMES[["TechPmult"]]) ->
      L203.TechPmult_CHINA

    L203.TechCoef_CHINA %>%
      filter(region!=gcamchina.REGION) %>%
      mutate(technology = "desalination",
             minicam.energy.input = gcamusa.WATER_TYPE_SEAWATER,
             market.name = gcamchina.REGION) %>%
      dplyr::filter(!is.na(year))->
      L203.TechDesalCoef_CHINA

    # Set shareweight of desalination technologies to 0 in all non-coastal states
    # and basins that do not come in contact with the ocean. This removes the possibility
    # of having desalination required in Texas, but coming from the Rio Grande which does not
    # have access to seawater without inland transportation.
    #
    # Additionally, desalination is now allowed for all sectors, including irrigation.
    # Given the price subsidy on agricultural water, desalination should never come
    # for irrigated agriculture as the price required would exceed the limits defined in
    # water_supply_constrained.xml
    L203.TechShrwt_CHINA %>%
      filter(region != gcamchina.REGION) %>%
      mutate(technology = "desalination",
             share.weight = if_else(!(region %in% seawater_provinces_basins), 0, 1))  %>%
      dplyr::filter(!is.na(year)) ->
      L203.TechDesalShrwt_CHINA

    L203.TechDesalShrwt_CHINA %>%
      rename(minicam.non.energy.input = share.weight) %>%
      mutate(minicam.non.energy.input = "final cost",
             input.cost = gcamusa.DESALINATION_PRICE) %>%
      dplyr::filter(!is.na(year)) ->
      L203.TechDesalCost_CHINA

    # ===================================================
    # Produce outputs
    L203.DeleteSupplysector_CHINA %>%
      add_title("Remove the water sectors from the China region that are produced at the province level") %>%
      add_units("Unitless") %>%
      add_comments("Remove the China electricity, municipal, and industrial water_td's") %>%
      add_comments("Also remove all energy-for-water (EFW) sectors") %>%
      add_precursors("L203.Supplysector_desal_basin",
                     "water/A71.sector",
                     "water/A72.sector",
                     "water/A73.sector",
                     "water/A74.sector") ->
      L203.DeleteSupplysector_CHINA

    L203.DeleteResTechInput_CHINA %>%
      add_title("Remove the electricity inputs to groundwater supply curves") %>%
      add_units("Unitless") %>%
      add_comments("These would be pulling from a China electricity market that does not exist in GCAM-China") %>%
      add_precursors("L201.RsrcTechCoef") ->
      L203.DeleteResTechInput_CHINA


    L203.DeleteSubsector_CHINA %>%
      add_title("Remove the three sectors that are produced at the province level") %>%
      add_units("Unitless") %>%
      add_comments("Remove the China electricity, municipal, and industrial water_td's") %>%
      add_legacy_name("L2232.DeleteSubsector_China") ->
      L203.DeleteSubsector_CHINA

    L203.Supplysector_CHINA %>%
      add_title("Water sector information") %>%
      add_units("Unitless") %>%
      add_comments("Supply sector info expanded to CHINA and province regions for water demand sectors") %>%
      add_legacy_name("L203.Supplysector") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L103.water_mapping_CHINA_R_LS_W_Ws_share",
                     "L103.water_mapping_CHINA_R_PRI_W_Ws_share",
                     "L103.water_mapping_CHINA_R_GLU_W_Ws_share",
                     "L103.water_mapping_CHINA_R_B_W_Ws_share",
                     "gcam-china/provinces_subregions",
                     "gcam-china/province_and_basin",
                     "water/water_td_sectors",
                     "water/A03.sector") ->
      L203.Supplysector_CHINA

    L203.SubsectorLogit_CHINA %>%
      add_title("Water subsector logit exponents for mapping sector") %>%
      add_units("Unitless") %>%
      add_comments("Subsector info expanded to CHINA and province regions for water demand sectors") %>%
      add_legacy_name("L203.SubsectorLogit") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L103.water_mapping_CHINA_R_LS_W_Ws_share",
                     "L103.water_mapping_CHINA_R_PRI_W_Ws_share",
                     "L103.water_mapping_CHINA_R_GLU_W_Ws_share",
                     "L103.water_mapping_CHINA_R_B_W_Ws_share",
                     "gcam-china/provinces_subregions",
                     "gcam-china/province_and_basin",
                     "water/water_td_sectors",
                     "water/A03.sector") ->
      L203.SubsectorLogit_CHINA

    L203.SubsectorShrwt_CHINA %>%
      add_title("Water subsector share weights") %>%
      add_units("Unitless") %>%
      add_comments("Subsector shareweights expanded to China and province regions for water demand sectors") %>%
      add_legacy_name("L203.SubsectorShrwtFllt") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L103.water_mapping_CHINA_R_LS_W_Ws_share",
                     "L103.water_mapping_CHINA_R_GLU_W_Ws_share",
                     "L103.water_mapping_CHINA_R_B_W_Ws_share",
                     "gcam-china/provinces_subregions",
                     "gcam-china/province_and_basin",
                     "water/water_td_sectors",
                     "water/A03.sector") ->
      L203.SubsectorShrwt_CHINA

    L203.TechShrwt_CHINA %>%
      add_title("Water technology shareweights") %>%
      add_units("Unitless") %>%
      add_comments("Technology shareweights expanded to China and province regions for water demand sectors") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L203.TechShrwt") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L103.water_mapping_CHINA_R_LS_W_Ws_share",
                     "L103.water_mapping_CHINA_R_GLU_W_Ws_share",
                     "L103.water_mapping_CHINA_R_B_W_Ws_share",
                     "gcam-china/provinces_subregions",
                     "gcam-china/province_and_basin",
                     "water/water_td_sectors",
                     "water/A03.sector") ->
      L203.TechShrwt_CHINA

    L203.TechCoef_CHINA%>%
      add_title("Water technology coefficients") %>%
      add_units("Unitless") %>%
      add_comments("Technology coefficients expanded to China and province regions for water demand sectors") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L103.water_mapping_CHINA_R_LS_W_Ws_share",
                     "L103.water_mapping_CHINA_R_GLU_W_Ws_share",
                     "L103.water_mapping_CHINA_R_B_W_Ws_share",
                     "gcam-china/provinces_subregions",
                     "gcam-china/province_and_basin",
                     "water/water_td_sectors",
                     "water/A03.sector") ->
      L203.TechCoef_CHINA

    L203.TechDesalCoef_CHINA %>%
      add_title("Water technology desal coefficients") %>%
      add_units("Unitless") %>%
      add_comments("Desalination Coefficients for China region and provinces. Available only for coastal provinces and basins") %>%
      add_legacy_name("L203.TechCoef") %>%
      same_precursors_as(L203.TechCoef_CHINA) ->
      L203.TechDesalCoef_CHINA

    L203.TechPmult_CHINA %>%
      add_title("Water technology price multipliers") %>%
      add_units("Unitless") %>%
      add_comments("Water price subsidy applied at CHINA and province level") %>%
      add_legacy_name("L203.TechCoef") %>%
      add_precursors("water/basin_to_country_mapping",
                     "L103.water_mapping_CHINA_R_LS_W_Ws_share",
                     "L103.water_mapping_CHINA_R_PRI_W_Ws_share",
                     "L103.water_mapping_CHINA_R_B_W_Ws_share",
                     "gcam-china/provinces_subregions",
                     "gcam-china/province_and_basin",
                     "water/water_td_sectors",
                     "water/A03.sector") ->
      L203.TechPmult_CHINA

    L203.TechDesalShrwt_CHINA %>%
      add_title("Water technology desal shareweights") %>%
      add_units("Unitless") %>%
      add_comments("Desalination Shareweights for China region and provinces. Available only for coastal provinces and basins") %>%
      add_legacy_name("L203.TechCoef") %>%
      same_precursors_as(L203.TechShrwt_CHINA) %>%
      add_precursors("gcam-china/china_seawater_provinces_basins") ->
      L203.TechDesalShrwt_CHINA

    L203.TechDesalCost_CHINA %>%
      add_title("Water technology desal costs") %>%
      add_units("Unitless") %>%
      add_comments("Desalination fixed costs") %>%
      add_legacy_name("L203.TechCoef") %>%
      same_precursors_as(L203.TechShrwt_CHINA) ->
      L203.TechDesalCost_CHINA

    return_data(L203.DeleteSupplysector_CHINA,
                L203.DeleteResTechInput_CHINA,
                L203.DeleteSubsector_CHINA,
                L203.Supplysector_CHINA,
                L203.SubsectorLogit_CHINA,
                L203.SubsectorShrwt_CHINA,
                L203.TechShrwt_CHINA,
                L203.TechCoef_CHINA,
                L203.TechDesalCoef_CHINA,
                L203.TechDesalShrwt_CHINA,
                L203.TechDesalCost_CHINA,
                L203.TechPmult_CHINA)
  } else {
    stop("Unknown command")
  }
}
