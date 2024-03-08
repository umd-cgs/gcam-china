# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L2235.elec_segments_GRID_CHINA
#'
#' Creates the vertical segment supplysectors in the grid regions and also the domestic supply and
#' electricity trade sectors.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2235.DeleteSupplysector_elec_CHINA}, \code{L2235.InterestRate_GRID_CHINA},
#' \code{L2235.Pop_GRID_CHINA}, \code{L2235.BaseGDP_GRID_CHINA}, \code{L2235.LaborForceFillout_GRID_CHINA},
#' \code{L2235.Supplysector_elec_CHINA}, \code{L2235.ElecReserve_elecS_grid_vertical_CHINA}, \code{L2235.SubsectorLogit_elec_CHINA},
#' \code{L2235.SubsectorShrwtFllt_elec_CHINA}, \code{L2235.SubsectorInterp_elec_CHINA},
#' \code{L2235.SubsectorShrwtFllt_elecS_grid_vertical_CHINA},\code{L2235.SubsectorShrwtInterp_elecS_grid_vertical_CHINA},
#' \code{L2235.TechShrwt_elec_CHINA}, \code{L2235.TechCoef_elec_CHINA}, \code{L2235.Production_exports_elec_CHINA},
#' \code{L2235.TechShrwt_elecS_grid_vertical_CHINA}, \code{L2235.TechCoef_elecS_grid_vertical_CHINA},
#' \code{L2235.Supplysector_elec_GRID_CHINA}, \code{L2235.SubsectorLogit_elec_GRID_CHINA}, \code{L2235.SubsectorShrwtFllt_elec_GRID_CHINA},
#' \code{L2235.SubsectorInterp_elec_GRID_CHINA}, \code{L2235.TechShrwt_elec_GRID_CHINA}, \code{L2235.TechCoef_elec_GRID_CHINA},
#' \code{L2235.TechCoef_elecownuse_GRID_CHINA}, \code{L2235.Production_imports_GRID_CHINA},\code{L2235.Production_elec_gen_GRID_CHINA}.
#' The corresponding file in the original data system was \code{L2235.elec_segments_FERC.R} (gcam-china level2).
#' @details This chunk generates input files to create the vertical segment supplysectors in the grid regions
#' and also the domestic supply and electricity trade sectors for the grid regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join distinct filter if_else mutate select
#' @importFrom tibble tibble
#' @author MTB Aug 2018 / YangOu Jul 2023
module_gcamchina_L2235.elec_segments_GRID_CHINA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "gcam-china/A232.structure",
             FILE = "gcam-china/A23.elec_delete",
             FILE = "gcam-china/A23.elecS_sector_vertical",
             FILE = "gcam-china/A23.elecS_metainfo_vertical",
             FILE = "gcam-china/ABSPI_intra_province_electricity_trade",
             "L1235.elecS_demand_fraction_CHINA",
             "L1235.elecS_horizontal_vertical_GCAM_coeff_CHINA",
             "L123.in_EJ_province_ownuse_elec",
             "L123.out_EJ_province_ownuse_elec",
             "L126.in_EJ_province_td_elec",
             "L132.out_EJ_province_indchp_F",
             "L1232.out_EJ_sR_elec_CHINA",
             "L126.in_EJ_R_electd_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2235.DeleteSupplysector_elec_CHINA",
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

  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    apply.to <- average.fossil.efficiency <- average.grid.capacity.factor <- backup.capacity.factor <-
      backup.capital.cost <- calOutputValue <- capacity.factor <- capacity.limit <- capital.overnight <- count_tech <-
      eff_actual <- efficiency <- Electric.sector <- Electric.sector.intermittent.technology <- electric.sector.market <-
      fixed.charge.rate <- fixedOutput <- flag <- from.year <- fraction <- GCAM_region_ID <- geothermal_resource <-
      grid_regions <- half.life <- input.capital <- input.OM.fixed <- input.OM.var <- input.unit <- intermittent.technology <-
      interpolation.function <- lifetime <- logit.exponent <- logit.type <- logit.year.fillout <- market.name <-
      median.shutdown.point <- minicam.energy.input <- minicam.non.energy.input <- OM.fixed <- OM.var <- output.unit <-
      price.unit <- profit.shutdown.steepness <- primary.renewable <- region <- remove.fraction <- sector.name <-
      share.weight <- share.weight.year <- steepness <- storage.market <- stub.technology <- subs.share.weight <-
      subscalOutputValue <- subsector <- subsector.cal.value <- subsector.name <- subsector_1 <- supplysector <-
      tech.share.weight <- technology <- to.value <- to.year <- trial.market.name <- type <- year <- grid_region <-
      vertical_segment <- demand_fraction <- coefficient <- subsector.logit <- year.fillout <- technology.logit <-
      technology.logit.type <- value <- state <- in.EJ <- out.EJ <- ownuse <- generation <- cogeneration <- consumption <-
      net.exports <- imports <- exports <- ownuse_coef <- net.supply <- NULL  # silence package check notes

    # Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings",strip_attributes = TRUE)
    A232.structure <- get_data(all_data, "gcam-china/A232.structure",strip_attributes = TRUE)
    A23.elec_delete <- get_data(all_data, "gcam-china/A23.elec_delete",strip_attributes = TRUE)
    A23.elecS_sector_vertical <- get_data(all_data, "gcam-china/A23.elecS_sector_vertical",strip_attributes = TRUE)
    A23.elecS_metainfo_vertical <- get_data(all_data, "gcam-china/A23.elecS_metainfo_vertical",strip_attributes = TRUE)
    ABSPI_intra_province_electricity_trade <- get_data(all_data, "gcam-china/ABSPI_intra_province_electricity_trade",strip_attributes = TRUE)
    L1235.elecS_demand_fraction_CHINA <- get_data(all_data, "L1235.elecS_demand_fraction_CHINA",strip_attributes = TRUE)
    L1235.elecS_horizontal_vertical_GCAM_coeff_CHINA <- get_data(all_data, "L1235.elecS_horizontal_vertical_GCAM_coeff_CHINA",strip_attributes = TRUE) %>%
      rename(region = grid_region)
    L123.in_EJ_province_ownuse_elec <- get_data(all_data, "L123.in_EJ_province_ownuse_elec",strip_attributes = TRUE)
    L123.out_EJ_province_ownuse_elec <- get_data(all_data, "L123.out_EJ_province_ownuse_elec",strip_attributes = TRUE)
    L126.in_EJ_province_td_elec <- get_data(all_data, "L126.in_EJ_province_td_elec",strip_attributes = TRUE)
    L132.out_EJ_province_indchp_F <- get_data(all_data, "L132.out_EJ_province_indchp_F",strip_attributes = TRUE)
    L1232.out_EJ_sR_elec_CHINA <- get_data(all_data, "L1232.out_EJ_sR_elec_CHINA",strip_attributes = TRUE)
    L126.in_EJ_R_electd_F_Yh <- get_data(all_data, "L126.in_EJ_R_electd_F_Yh",strip_attributes = TRUE)

    # ===================================================
    # Data Processing

    # PART 1: THE CHINA REGION
    # Remove the CHINA electricity sector, and replace with electricity trade
    A23.elec_delete %>%
      mutate(region = gcamchina.REGION,
             region = region) -> L2235.DeleteSupplysector_elec_CHINA

    # Creating vertical segments in grid regions
    L2235.Supplysector_elecS_grid_vertical <-
      write_to_all_provinces(A23.elecS_sector_vertical,
                             c("region", "supplysector", "output.unit", "input.unit", "price.unit",
                               "logit.year.fillout", "logit.exponent" , "logit.type"),
                             # NOTE: writing to all grid regions, rather than provinces
                             gcamchina.GRID_REGIONS)

    L2235.ElecReserve_elecS_grid_vertical <-
      write_to_all_provinces(A23.elecS_metainfo_vertical %>%
                               select(-region),
                             c("region", "supplysector","electricity.reserve.margin",
                               "average.grid.capacity.factor"),
                             # NOTE: writing to all grid regions, rather than provinces
                             gcamchina.GRID_REGIONS)

    # Subsector share-weights for vertical segment supplysectors
    L2235.Supplysector_elecS_grid_vertical %>%
      select(region, supplysector) %>%
      mutate(subsector = supplysector,
             logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = gcamchina.GRID.REGION_LOGIT,
             logit.type = gcamchina.GRID.REGION_LOGIT_TYPE) -> L2235.SubsectorLogit_elecS_grid_vertical

    L2235.SubsectorLogit_elecS_grid_vertical %>%
      select(region, supplysector, subsector) %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS),
             share.weight = gcamchina.DEFAULT_SHAREWEIGHT) -> L2235.SubsectorShrwtFllt_elecS_grid_vertical

    L2235.SubsectorLogit_elecS_grid_vertical %>%
      select(region, supplysector, subsector) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_FUTURE_YEARS),
             interpolation.function = "fixed") -> L2235.SubsectorShrwtInterp_elecS_grid_vertical

    # Technology shareweights
    L2235.SubsectorLogit_elecS_grid_vertical %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subsector,
             share.weight = gcamchina.DEFAULT_SHAREWEIGHT) %>%
      select(region, supplysector, subsector, technology, year, share.weight) -> L2235.TechShrwt_elecS_grid_vertical

    # Technology inputs
    L1235.elecS_demand_fraction_CHINA %>%
      mutate(supplysector = "electricity",
             subsector = supplysector,
             technology = supplysector,
             market.name = grid_region) %>%
      rename(region = grid_region,
             minicam.energy.input = vertical_segment) %>%
      select(-demand_fraction) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) -> L2235.TechMarket_elecS_grid_vertical_electricity

    L1235.elecS_horizontal_vertical_GCAM_coeff_CHINA %>%
      mutate(market.name = region) %>%
      select(-coefficient) %>%
      arrange(region) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) -> L2235.TechMarket_elecS_grid_vertical

    L2235.TechMarket_elecS_grid_vertical %>%
      bind_rows(L2235.TechMarket_elecS_grid_vertical_electricity) -> L2235.TechMarket_elecS_grid_vertical

    # Coefficients for horizontal to vertical segments.
    L2235.TechMarket_elecS_grid_vertical_electricity %>%
      # MB note:  document why no LJENM
      left_join(L1235.elecS_demand_fraction_CHINA, by = c("region" = "grid_region",
                                                          "minicam.energy.input" = "vertical_segment")) %>%
      rename(coefficient = demand_fraction) -> L2235.TechCoef_elecS_grid_vertical_electricity

    L2235.TechMarket_elecS_grid_vertical %>%
      # Some non-load segment supplysectors still exist, filter those here
      filter(supplysector!="electricity") %>%
      left_join_error_no_match(L1235.elecS_horizontal_vertical_GCAM_coeff_CHINA,
                               by = c("region", "supplysector", "subsector", "technology", "minicam.energy.input")) %>%
      select(region, supplysector, subsector, technology, year, minicam.energy.input, coefficient, market.name) %>%
      bind_rows(L2235.TechCoef_elecS_grid_vertical_electricity) -> L2235.TechCoef_elecS_grid_vertical

    # Supplysector for electricity sector in the China region, including logit exponent between grid regions
    # All of the supplysector information is the same as before, except the logit exponent
    A232.structure %>%
      filter(region == gcamchina.REGION) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.type = gcamchina.GRID.REGION_LOGIT_TYPE) %>%
      select(region, supplysector, output.unit, input.unit, price.unit,
             logit.year.fillout, logit.exponent = subsector.logit, logit.type) -> L2235.Supplysector_elec_CHINA

    # Append vertical segments to the supplysector and subsector logit tables
    L2235.Supplysector_elecS_grid_vertical %>%
      mutate(logit.year.fillout = as.numeric(logit.year.fillout)) %>%
      bind_rows(L2235.Supplysector_elec_CHINA) -> L2235.Supplysector_elec_CHINA

    # No need to read in subsector logit exponents, which are applied to the technology competition
    A232.structure %>%
      filter(region == gcamchina.REGION) %>%
      repeat_add_columns(tibble(grid_region = gcamchina.GRID_REGIONS)) %>%
      mutate(subsector = gsub("grid_region", "", subsector),
             subsector = paste0(grid_region, subsector),
             year.fillout = min(MODEL_BASE_YEARS),
             share.weight = gcamchina.DEFAULT_SHAREWEIGHT) %>%
      select(region, supplysector, subsector, year.fillout, share.weight) -> L2235.SubsectorShrwtFllt_elec_CHINA

    # Subsector (grid region) shareweights in China electricity
    # NOTE: this just carries the base year shareweights forward;
    # regions that don't export in the base year don't export at all
    L2235.SubsectorShrwtFllt_elec_CHINA %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") %>%
      select(-year.fillout, -share.weight) %>%
      mutate(from.year = as.integer(from.year),
             to.year = as.integer(to.year)) -> L2235.SubsectorInterp_elec_CHINA

    # NOTE: There is only one tech per subsector in the grid markets so the logit choice does not matter
    L2235.SubsectorShrwtFllt_elec_CHINA %>%
      select(-share.weight) %>%
      # joined table includes an NA; left_join_error_no_match throws error, so left_join is used
      left_join(A232.structure %>%
                  select(region, supplysector, technology.logit, technology.logit.type),
                by = c("region", "supplysector")) %>%
      rename(logit.year.fillout = year.fillout,
             logit.exponent = technology.logit,
             logit.type = technology.logit.type) -> L2235.SubsectorLogit_elec_CHINA

    L2235.SubsectorLogit_elecS_grid_vertical %>%
      mutate(logit.year.fillout = as.numeric(logit.year.fillout)) %>%
      bind_rows(L2235.SubsectorLogit_elec_CHINA) -> L2235.SubsectorLogit_elec_CHINA

    # Technology shareweights, CHINA region
    A232.structure %>%
      filter(region == gcamchina.REGION) %>%
      repeat_add_columns(tibble(grid_region = gcamchina.GRID_REGIONS)) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(subsector = gsub("grid_region", "", subsector),
             subsector = paste0(grid_region, subsector),
             technology = gsub("grid_region", "", technology),
             technology = paste0(grid_region, technology),
             share.weight = gcamchina.DEFAULT_SHAREWEIGHT) %>%
      select(region, supplysector, subsector, technology, year, share.weight, grid_region) -> L2235.TechShrwt_elec_CHINA

    # Technology coefficients and market names, CHINA region
    L2235.TechShrwt_elec_CHINA %>%
      left_join_error_no_match(A232.structure %>% select(region, supplysector, minicam.energy.input),
                               by = c("region", "supplysector")) %>%
      mutate(coefficient = gcamchina.DEFAULT_COEFFICIENT,
             market.name = grid_region) %>%
      select(region, supplysector, subsector, technology, year,
             minicam.energy.input, coefficient, market.name) -> L2235.TechCoef_elec_CHINA

    # Compiling flows of electricity in each grid region: generation, cogeneration, ownuse, and consumption by all sectors
    L2235.TechShrwt_elec_CHINA %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L1232.out_EJ_sR_elec_CHINA %>%
                                 select(grid_region = grid.region, year, generation = value),
                               by = c("grid_region", "year")) %>%
      mutate(region = gcamchina.REGION) -> L2235.elec_flows_GRID_temp

    # Cogeneration is not included in the grid region totals; need to add it here for balance
    L132.out_EJ_province_indchp_F %>%
      left_join_error_no_match(province_names_mappings %>%
                                 select(province, grid_region = grid.region),
                               by = "province") %>%
      group_by(grid_region, year) %>%
      summarise(cogeneration = sum(value)) %>%
      ungroup() -> L2235.elec_flows_GRID_CHP

    # Subtract own use of electricity in each grid region prior to calculating net exports
    L123.in_EJ_province_ownuse_elec %>%
      rename(in.EJ = value) %>%
      left_join_error_no_match(L123.out_EJ_province_ownuse_elec %>%
                                 rename(out.EJ = value),
                               by = c("province", "sector", "fuel", "year")) %>%
      left_join_error_no_match(province_names_mappings %>%
                                 select(province, grid_region = grid.region),
                               by = "province") %>%
      mutate(ownuse = in.EJ - out.EJ) %>%
      group_by(grid_region, year) %>%
      summarise(ownuse = sum(ownuse)) %>%
      ungroup() -> L2235.net_EJ_province_ownuse_elec

    # Aggregate final electricity demands for each grid region
    # (i.e. the demands of all elect_td sectors - building, industry, transportation - for every state in a given grid region)
    L126.in_EJ_province_td_elec %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(province_names_mappings %>%
                                 select(province, grid_region = grid.region),
                               by = "province") %>%
      group_by(grid_region, year) %>%
      summarise(consumption = sum(value)) %>%
      ungroup() -> L2235.in_EJ_province_td_elec_temp

    # 2023 Nov Yang Ou
    # adjust electricity TD to align with the global model, so that between-grid electricity trade can be balanced
    # --------------------------------------------------------------------------------
    L126.in_EJ_R_electd_F_Yh %>%
      filter(GCAM_region_ID == gcamchina.REGION_ID & fuel == "electricity") %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      rename(value.core = value) %>%
      left_join_error_no_match(L2235.in_EJ_province_td_elec_temp %>%
                                 group_by(year) %>%
                                 summarise(value.gcamchina = sum(consumption)) %>% ungroup, by = "year") %>%
      mutate(scalar = if_else(year == 1975, 0.8375, value.core / value.gcamchina)) %>%
      select(year, scalar) -> L126.in_EJ_R_electd_F_Yh_scalar

    L2235.in_EJ_province_td_elec_temp %>%
      left_join_error_no_match(L126.in_EJ_R_electd_F_Yh_scalar, by = "year") %>%
      mutate(consumption = consumption * scalar) %>%
      select(-scalar) -> L2235.in_EJ_province_td_elec
    # --------------------------------------------------------------------------------

    # Calculating net exports: generation + cogeneration - ownuse - consumption
    L2235.elec_flows_GRID_temp %>%
      left_join_error_no_match(L2235.elec_flows_GRID_CHP,
                               by = c("grid_region", "year")) %>%
      left_join_error_no_match(L2235.net_EJ_province_ownuse_elec,
                               by = c("grid_region", "year")) %>%
      left_join_error_no_match(L2235.in_EJ_province_td_elec,
                               by = c("grid_region", "year")) %>%
      mutate(net.exports = generation + cogeneration - ownuse - consumption,
             # Split net exports into gross imports and exports
             imports = pmax(0, -1 * net.exports),
             exports = pmax(0, net.exports),
             # Calculate consumption from domestic sources: total consumption minus imports
             net.supply = consumption - imports) -> L2235.elec_flows_GRID

    # We have thus far assumed that electricity trade from and to grid regions
    # is based on the net trade, however in most cases regions will import and
    # export electricity from the grid at the same time which was not being captured.
    # We add here a method to reverse engineer calibrated gross trade values
    # that will account for imports and exports from the grid region.
    # YangOu: July 2023
    # Following GCAM-USA's approach, based on China's electricity trade data from
    # Annual Book of Statistics for Power Industry


    ABSPI_intra_province_electricity_trade %>%
      dplyr::rename(import_province = to.region,
                    export_province = from.region) %>%
      ## Certain to.region is labelled as "Export" to neighbor countries, drop
      filter(import_province != "Export") %>%
      left_join_error_no_match(province_names_mappings %>%
                                 select(province, grid_region = grid.region),
                               by=c("import_province" = "province")) %>%
      rename(import_grid = grid_region) %>%
      left_join_error_no_match(province_names_mappings %>%
                                 select(province, grid_region = grid.region),
                               by=c("export_province" = "province")) %>%
      rename(export_grid = grid_region) %>%
      # Filter for trading between different grids
      filter(export_grid != import_grid) %>%
      select(Year = year, Received_Electricity = value, import_grid, export_grid) ->
      grid_trade

    ## Calculate gross imports in each grid
    grid_trade %>%
      select(-export_grid) %>%
      group_by(Year,import_grid) %>%
      summarise(grid_imports = sum(Received_Electricity)) %>%
      ungroup() ->
      grid_imports

    ## Calculate gross exports in each grid
    grid_trade %>%
      select(-import_grid) %>%
      group_by(Year,export_grid) %>%
      summarise(grid_exports = sum(Received_Electricity)) %>%
      ungroup() ->
      grid_exports

    ## Calculate import to export ratio and make adjustments to calibrated values
    grid_imports %>%
      full_join(grid_exports %>%
                  complete(nesting(export_grid), Year = MODEL_BASE_YEARS) %>%
                  group_by(export_grid) %>%
                  mutate(grid_exports = approx_fun(Year, grid_exports, rule = 2)) %>%
                  filter(Year %in% grid_imports$Year),
                by = c("Year", "import_grid" = "export_grid")) %>%
      ## If grids do not have imports or exports, a NA will occur,
      ## we replace these with zeros to understand that 100% share is
      ## imports or exports respectively.
      replace_na(list(grid_exports=0,grid_imports=0)) %>%
      mutate(i2e_ratio = grid_imports / (grid_exports + grid_imports),
             e2i_ratio = 1 - i2e_ratio) %>%
      filter(Year %in% MODEL_BASE_YEARS) ->
      grid_trade_ratio

    ## Data exists from Annual Book of Statistics for Power Industry only back to 2005, therefore
    ## we hold 2005 ratios constant in past
    grid_trade_ratio %>%
      # fill the table with all other base years
      complete(nesting(import_grid), Year = MODEL_BASE_YEARS) %>%
      group_by(import_grid) %>%
      # hold 2010 values constant for all earlier base years
      mutate(grid_imports = approx_fun(Year, grid_imports, rule = 2),
             grid_exports = approx_fun(Year, grid_exports, rule = 2),
             i2e_ratio = approx_fun(Year, i2e_ratio, rule = 2),
             e2i_ratio = approx_fun(Year, e2i_ratio, rule = 2)) %>%
      ungroup() %>%
      arrange(Year) %>%
      select(-grid_exports, -grid_imports) ->
      grid_trade_ratio

    # TODO: the current trade flow data have some issues, need to find a better source

    # L2235.elec_flows_GRID %>%
    #   left_join_error_no_match(grid_trade_ratio, by=c("year"="Year","grid_region"="import_grid")) %>%
    #   mutate(
    #     imports_new = if_else( imports > 0, imports/i2e_ratio,
    #                            if_else( imports == 0, (exports/e2i_ratio)-exports, imports)),
    #     exports_new = if_else( exports >0, exports/e2i_ratio,
    #                            if_else( exports == 0, (imports/i2e_ratio)-imports, exports)),
    #     ## The data does not line up in the New York Grid yielding INF so we revert back to previous trade
    #     imports_new = if_else(imports_new >50,imports,imports_new),
    #     exports_new = if_else(exports_new >50,exports,exports_new)) %>%
    #   select(-imports,-exports, -i2e_ratio, -e2i_ratio) %>%
    #   rename(exports=exports_new,
    #          imports=imports_new) %>%
    #   ## Reassign net.supply to be used as net ownuse to reflect change
    #   ## in imports by grid region
    #   mutate(net.supply = consumption - imports) ->
    #   L2235.elec_flows_GRID

    # Calibrated exports of electricity from grid regions to shared CHINA region
    L2235.elec_flows_GRID %>%
      mutate(calOutputValue = round(exports, gcamchina.DIGITS_CALOUTPUT),
             share.weight.year = year,
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      set_subsector_shrwt() %>%
      select(region, supplysector, subsector, technology, year, calOutputValue, share.weight.year,
             subs.share.weight, tech.share.weight) -> L2235.Production_exports_elec_CHINA


    # PART 2: THE GRID REGIONS
    # Interest rates in the GRID grid regions
    province_names_mappings %>%
      distinct(grid.region) %>%
      mutate(interest.rate = socioeconomics.DEFAULT_INTEREST_RATE) %>%
      rename(region = grid.region) %>%
      arrange(region) -> L2235.InterestRate_GRID_CHINA

    # Population rates in the GRID grid regions
    province_names_mappings %>%
      distinct(grid.region) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(totalPop = 1) %>%
      rename(region = grid.region) %>%
      arrange(region) -> L2235.Pop_GRID_CHINA

    # Base GDP in GRID regions
    province_names_mappings %>%
      distinct(grid.region) %>%
      mutate(baseGDP = 1) %>%
      rename(region = grid.region) %>%
      arrange(region) -> L2235.BaseGDP_GRID_CHINA

    # Labor force in the GRID regions
    province_names_mappings %>%
      distinct(grid.region) %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS),
             laborforce = socioeconomics.DEFAULT_LABORFORCE) %>%
      rename(region = grid.region) %>%
      arrange(region) -> L2235.LaborForceFillout_GRID_CHINA

    # Supplysector information for electricity passthru sectors in the GRID regions
    A232.structure %>%
      filter(region == "grid_region") %>%
      select(-region) %>%
      repeat_add_columns(tibble(region = gcamchina.GRID_REGIONS)) %>%
      mutate(market.name = if_else(market.name == "grid_region", region, market.name)) -> L2235.structure_GRID

    L2235.structure_GRID %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.type = gcamchina.GRID.REGION_LOGIT_TYPE) %>%
      select(region, supplysector, output.unit, input.unit, price.unit,
             logit.year.fillout, logit.exponent = subsector.logit, logit.type) ->
      L2235.Supplysector_elec_GRID

    # Subsector (grid region) shareweights in China electricity
    L2235.structure_GRID %>%
      select(region, supplysector, subsector) %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS),
             share.weight = gcamchina.DEFAULT_SHAREWEIGHT) -> L2235.SubsectorShrwtFllt_elec_GRID

    # Subsector (grid region) shareweights in CHINA electricity
    L2235.structure_GRID %>%
      select(region, supplysector, subsector) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") %>%
      mutate(from.year = as.integer(from.year),
             to.year = as.integer(to.year)) -> L2235.SubsectorInterp_elec_GRID

    # NOTE: There is only one tech per subsector in the FERC markets so the logit choice does not matter
    L2235.structure_GRID %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.type = gcamchina.GRID.REGION_LOGIT_TYPE) %>%
      select(region, supplysector, subsector, logit.year.fillout,
             logit.exponent = technology.logit, logit.type) -> L2235.SubsectorLogit_elec_GRID

    # Technology shareweights, CHINA region
    L2235.structure_GRID %>%
      select(region, supplysector, subsector, technology) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = gcamchina.DEFAULT_SHAREWEIGHT) -> L2235.TechShrwt_elec_GRID

    # Technology coefficients and market names
    L2235.structure_GRID %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      filter(supplysector != "electricity_net_ownuse") %>%
      mutate(coefficient = gcamchina.DEFAULT_COEFFICIENT) %>%
      select(region, supplysector, subsector, technology, year, minicam.energy.input,
             coefficient, market.name) -> L2235.TechCoef_elec_GRID

    # Own use coefficients in the grid regions
    L2235.structure_GRID %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      filter(supplysector == "electricity_net_ownuse") %>%
      # join will produce NAs for logit type columns; left_join_error_no_match throws error, so left_join is used
      left_join(L2235.elec_flows_GRID %>%
                  mutate(ownuse_coef = (generation + cogeneration) / (generation + cogeneration - ownuse)) %>%
                  select(grid_region, year, ownuse_coef),
                by = c("region" = "grid_region", "year")) %>%
      group_by(region) %>%
      mutate(coefficient = if_else(is.na(ownuse_coef), ownuse_coef[year==max(MODEL_BASE_YEARS)], ownuse_coef)) %>%
      ungroup() %>%
      select(region, supplysector, subsector, technology, year, minicam.energy.input,
             coefficient, market.name) -> L2235.TechCoef_elecownuse_GRID

    # Calibrated electricity imports (from CHINA region)
    L2235.TechCoef_elec_GRID %>%
      filter(year %in% MODEL_BASE_YEARS,
             market.name == gcamchina.REGION) %>%
      select(region, supplysector, subsector, technology, year) %>%
      left_join_error_no_match(L2235.elec_flows_GRID %>%
                                 select(grid_region, year, imports),
                               by = c("region" = "grid_region", "year")) %>%
      mutate(calOutputValue = round(imports, gcamchina.DIGITS_CALOUTPUT),
             share.weight.year = year) %>%
      set_subsector_shrwt() %>%
      mutate(tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      select(-imports) -> L2235.Production_imports_GRID

    # Calibrated net electricity generation (from within grid region)
    L2235.TechCoef_elec_GRID%>%
      filter(year %in% MODEL_BASE_YEARS,
             market.name != gcamchina.REGION) %>%
      select(region, supplysector, subsector, technology, year) %>%
      left_join_error_no_match(L2235.elec_flows_GRID %>%
                                 select(grid_region, year, net.supply),
                               by = c("region" = "grid_region", "year")) %>%
      mutate(calOutputValue = round(net.supply, gcamchina.DIGITS_CALOUTPUT),
             share.weight.year = year) %>%
      set_subsector_shrwt() %>%
      mutate(tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      select(-net.supply) -> L2235.Production_elec_gen_GRID


    # ===================================================
    # Produce outputs

    L2235.DeleteSupplysector_elec_CHINA %>%
      add_title("China Electricity Sectors to be Deleted") %>%
      add_units("NA") %>%
      add_comments("Removing the China region electricity sectors (incl. net_ownuse)") %>%
      add_legacy_name("L2235.DeleteSupplysector_CHINAelec") %>%
      add_precursors("gcam-china/A23.elec_delete") ->
      L2235.DeleteSupplysector_elec_CHINA

    L2235.InterestRate_GRID_CHINA %>%
      add_title("Grid Region Interest Rates") %>%
      add_units("unitless") %>%
      add_comments("Interest rates in the grid regions") %>%
      add_comments("Default interest rate assumption used") %>%
      add_legacy_name("L2235.InterestRate_FERC") %>%
      add_precursors("gcam-china/province_names_mappings") ->
      L2235.InterestRate_GRID_CHINA

    L2235.Pop_GRID_CHINA %>%
      add_title("Grid Region Populations") %>%
      add_units("thousand persons") %>%
      add_comments("Population in the grid regions") %>%
      add_comments("Value is arbitrary and does not matter; but a value must be read in") %>%
      add_legacy_name("L2235.Pop_FERC") %>%
      add_precursors("gcam-china/province_names_mappings") ->
      L2235.Pop_GRID_CHINA

    L2235.BaseGDP_GRID_CHINA %>%
      add_title("Grid Region Base Year GDP") %>%
      add_units("million 1990 USD") %>%
      add_comments("Base year GDP in the grid regions") %>%
      add_comments("Value is arbitrary and does not matter; but a value must be read in") %>%
      add_legacy_name("L2235.BaseGDP_FERC") %>%
      add_precursors("gcam-china/province_names_mappings") ->
      L2235.BaseGDP_GRID_CHINA

    L2235.LaborForceFillout_GRID_CHINA %>%
      add_title("Grid Region Labor Force") %>%
      add_units("unitless") %>%
      add_comments("Labor force in the grid regions") %>%
      add_comments("Default labor force assumption used") %>%
      add_legacy_name("L2235.LaborForceFillout_FERC") %>%
      add_precursors("gcam-china/province_names_mappings") ->
      L2235.LaborForceFillout_GRID_CHINA

    L2235.Supplysector_elec_CHINA %>%
      add_title("Electricity Supply Sectors for China Electricity Trade and Grid Region Vertical Load Segments") %>%
      add_units("unitless") %>%
      add_comments("Electricity supply sector in the China region and grid regions; including trade between grid regions") %>%
      add_legacy_name("L2235.Supplysector_CHINAelec") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/A23.elecS_sector_vertical",
                     "gcam-china/A232.structure") ->
      L2235.Supplysector_elec_CHINA

    L2235.ElecReserve_elecS_grid_vertical %>%
      add_title("Grid Region Vertical Electricity Load Segments Information") %>%
      add_units("unitless") %>%
      add_comments("Grid region vertical electricity load segment reserve margin and capacity factors") %>%
      add_legacy_name("L2235.ElecReserve_elecS_grid_vertical") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/A23.elecS_metainfo_vertical") ->
      L2235.ElecReserve_elecS_grid_vertical_CHINA

    L2235.SubsectorLogit_elec_CHINA %>%
      add_title("Vertical Load Segments Subsector Logits") %>%
      add_units("unitless") %>%
      add_comments("Vertical electricity load segments subsector logits") %>%
      add_legacy_name("L2235.SubsectorLogit_elec_CHINA") %>%
      same_precursors_as("L2235.Supplysector_elec_CHINA") ->
      L2235.SubsectorLogit_elec_CHINA

    L2235.SubsectorShrwtFllt_elec_CHINA %>%
      add_title("China Electricity Trade Subsector Share Weights") %>%
      add_units("unitless") %>%
      add_comments("China electricity trade subsector (grid region) share weights") %>%
      add_legacy_name("L2235.SubsectorShrwtFllt_elec_CHINA") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/A232.structure") ->
      L2235.SubsectorShrwtFllt_elec_CHINA

    L2235.SubsectorInterp_elec_CHINA %>%
      add_title("China Electricity Trade Subsector Share Weights") %>%
      add_units("unitless") %>%
      add_comments("China electricity trade subsector (grid region) share weights that are fixed at calibration values") %>%
      add_comments("Grid regions that don't export in the base year don't export at all") %>%
      add_legacy_name("L2235.SubsectorInterp_elec_CHINA") %>%
      same_precursors_as("L2235.SubsectorShrwtFllt_elec_CHINA") ->
      L2235.SubsectorInterp_elec_CHINA

    L2235.SubsectorShrwtFllt_elecS_grid_vertical %>%
      add_title("Grid Region Vertical Load Segments Subsector Share Weights") %>%
      add_units("unitless") %>%
      add_comments("Vertical electricity load segments subsector share weights for the grid regions") %>%
      add_legacy_name("L2235.SubsectorShrwtFllt_elecS_grid_vertical") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/A23.elecS_sector_vertical") ->
      L2235.SubsectorShrwtFllt_elecS_grid_vertical_CHINA

    L2235.SubsectorShrwtInterp_elecS_grid_vertical %>%
      add_title("Grid Region Vertical Load Segments Subsector Share Weights") %>%
      add_units("unitless") %>%
      add_comments("Vertical electricity load segments subsector share weights for the grid regions that are fixed at calibration values") %>%
      add_legacy_name("L2235.SubsectorShrwtInterp_elecS_grid_vertical") %>%
      same_precursors_as("L2235.SubsectorShrwtFllt_elecS_grid_vertical_CHINA") ->
      L2235.SubsectorShrwtInterp_elecS_grid_vertical_CHINA

    L2235.TechShrwt_elec_CHINA %>%
      add_title("China Electricity Trade Technology Share Weights") %>%
      add_units("unitless") %>%
      add_comments("China electricity trade technology share weights") %>%
      add_legacy_name("L2235.TechShrwt_elec_CHINA") %>%
      same_precursors_as("L2235.SubsectorShrwtFllt_elec_CHINA")  ->
      L2235.TechShrwt_elec_CHINA

    L2235.TechCoef_elec_CHINA %>%
      add_title("China Electricity Trade Technology Market Info") %>%
      add_units("unitless") %>%
      add_comments("China electricity trade technology coefficients and market names") %>%
      add_legacy_name("L2235.TechCoef_elec_CHINA") %>%
      same_precursors_as("L2235.SubsectorShrwtFllt_elec_CHINA") ->
      L2235.TechCoef_elec_CHINA

    L2235.Production_exports_elec_CHINA %>%
      add_title("Electricity Exports from Grid Regions to China Electricity Trade Sector") %>%
      add_units("EJ (calOutputValue); unitless") %>%
      add_comments("Calibrated electricity exports from grid regions to China region electricity trade sector") %>%
      add_legacy_name("L2235.Production_exports_elec_CHINA") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/A232.structure",
                     "gcam-china/ABSPI_intra_province_electricity_trade",
                     "L126.in_EJ_R_electd_F_Yh",
                     "L1232.out_EJ_sR_elec_CHINA",
                     "L132.out_EJ_province_indchp_F",
                     "L123.in_EJ_province_ownuse_elec",
                     "L123.out_EJ_province_ownuse_elec",
                     "L126.in_EJ_province_td_elec") ->
      L2235.Production_exports_elec_CHINA

    L2235.TechShrwt_elecS_grid_vertical %>%
      add_title("Grid Region Vertical Load Segments Technology Share Weights") %>%
      add_units("unitless") %>%
      add_comments("Vertical electricity load segments technology share weights for the grid regions") %>%
      add_legacy_name("L2235.TechShrwt_elecS_grid_vertical") %>%
      same_precursors_as("L2235.SubsectorShrwtFllt_elecS_grid_vertical_USA") ->
      L2235.TechShrwt_elecS_grid_vertical_CHINA

    L2235.TechCoef_elecS_grid_vertical %>%
      add_title("Grid Region Vertical Load Segments Technology Market Info") %>%
      add_units("unitless") %>%
      add_comments("Vertical electricity load segments technology coefficients and market names") %>%
      add_legacy_name("L2235.TechCoef_elecS_grid_vertical") %>%
      add_precursors("L1235.elecS_horizontal_vertical_GCAM_coeff_CHINA",
                     "L1235.elecS_demand_fraction_CHINA") ->
      L2235.TechCoef_elecS_grid_vertical_CHINA

    L2235.Supplysector_elec_GRID %>%
      add_title("Grid Region Electricity Passthrough Sector Information") %>%
      add_units("unitless") %>%
      add_comments("Supply sector information for electricity passthrough sectors in the grid regions") %>%
      add_legacy_name("L2235.Supplysector_elec_GRID") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/A232.structure") ->
      L2235.Supplysector_elec_GRID_CHINA

    L2235.SubsectorLogit_elec_GRID %>%
      add_title("Grid Region Electricity Passthrough Subsector Logits") %>%
      add_units("unitless") %>%
      add_comments("Subsector logits for electricity passthrough sectors in the grid regions") %>%
      add_legacy_name("L2235.SubsectorShrwtFllt_elec_GRID") %>%
      same_precursors_as("L2235.Supplysector_elec_GRID_CHINA") ->
      L2235.SubsectorLogit_elec_GRID_CHINA

    L2235.SubsectorShrwtFllt_elec_GRID %>%
      add_title("Grid Region Electricity Passthrough Subsector Share Weights") %>%
      add_units("unitless") %>%
      add_comments("Subsector share weights for electricity passthrough sectors in the grid regions") %>%
      add_legacy_name("L2235.SubsectorShrwtFllt_elec_GRID") %>%
      same_precursors_as("L2235.Supplysector_elec_GRID_CHINA") ->
      L2235.SubsectorShrwtFllt_elec_GRID_CHINA

    L2235.SubsectorInterp_elec_GRID %>%
      add_title("Grid Region Electricity Passthrough Subsector Share Weights") %>%
      add_units("unitless") %>%
      add_comments("Subsector share weights for electricity passthrough sectors in the grid regions that are fixed at calibration values") %>%
      add_legacy_name("L2235.SubsectorInterp_elec_GRID") %>%
      same_precursors_as("L2235.Supplysector_elec_GRID_CHINA") ->
      L2235.SubsectorInterp_elec_GRID_CHINA

    L2235.TechShrwt_elec_GRID %>%
      add_title("Grid Region Electricity Passthrough Technology Share Weights") %>%
      add_units("unitless") %>%
      add_comments("Technology share weights for electricity passthrough sectors in the grid regions") %>%
      add_legacy_name("L2235.TechShrwt_elec_GRID") %>%
      same_precursors_as("L2235.Supplysector_elec_GRID_CHINA") ->
      L2235.TechShrwt_elec_GRID_CHINA

    L2235.TechCoef_elec_GRID %>%
      add_title("Grid Region Electricity Passthrough Technology Market Info") %>%
      add_units("unitless") %>%
      add_comments("Electricity passthrough technology coefficients and market names in the grid regions") %>%
      add_legacy_name("L2235.TechCoef_elec_GRID") %>%
      same_precursors_as("L2235.Supplysector_elec_GRID_CHINA") ->
      L2235.TechCoef_elec_GRID_CHINA

    L2235.TechCoef_elecownuse_GRID %>%
      add_title("Grid Region Electricity Own Use Market Info") %>%
      add_units("unitless") %>%
      add_comments("Electricity own use coefficients and market names in the grid regions") %>%
      add_legacy_name("L2235.TechCoef_elecownuse_GRID") %>%
      same_precursors_as("L2235.Production_exports_elec_CHINA") ->
      L2235.TechCoef_elecownuse_GRID_CHINA

    L2235.Production_imports_GRID %>%
      add_title("Electricity Imports from China Electricity Trade Sector to Grid Regions") %>%
      add_units("EJ (calOutputValue); unitless") %>%
      add_comments("Calibrated electricity imports from China region electricity trade sector to the grid regions") %>%
      add_legacy_name("L2235.Production_imports_GRID") %>%
      same_precursors_as("L2235.Production_exports_elec_CHINA") ->
      L2235.Production_imports_GRID_CHINA

    L2235.Production_elec_gen_GRID %>%
      add_title("Electricity Generation by Grid Region in Base Years") %>%
      add_units("EJ (calOutputValue); unitless") %>%
      add_comments("Calibrated net electricity generation from within the grid regions") %>%
      add_legacy_name("L2235.Production_elec_gen_GRID") %>%
      same_precursors_as("L2235.Production_exports_elec_CHINA") ->
      L2235.Production_elec_gen_GRID_CHINA

    return_data(L2235.DeleteSupplysector_elec_CHINA,
                L2235.InterestRate_GRID_CHINA,
                L2235.Pop_GRID_CHINA,
                L2235.BaseGDP_GRID_CHINA,
                L2235.LaborForceFillout_GRID_CHINA,
                L2235.Supplysector_elec_CHINA,
                L2235.ElecReserve_elecS_grid_vertical_CHINA,
                L2235.SubsectorLogit_elec_CHINA,
                L2235.SubsectorShrwtFllt_elec_CHINA,
                L2235.SubsectorInterp_elec_CHINA,
                L2235.SubsectorShrwtFllt_elecS_grid_vertical_CHINA,
                L2235.SubsectorShrwtInterp_elecS_grid_vertical_CHINA,
                L2235.TechShrwt_elec_CHINA,
                L2235.TechCoef_elec_CHINA,
                L2235.Production_exports_elec_CHINA,
                L2235.TechShrwt_elecS_grid_vertical_CHINA,
                L2235.TechCoef_elecS_grid_vertical_CHINA,
                L2235.Supplysector_elec_GRID_CHINA,
                L2235.SubsectorLogit_elec_GRID_CHINA,
                L2235.SubsectorShrwtFllt_elec_GRID_CHINA,
                L2235.SubsectorInterp_elec_GRID_CHINA,
                L2235.TechShrwt_elec_GRID_CHINA,
                L2235.TechCoef_elec_GRID_CHINA,
                L2235.TechCoef_elecownuse_GRID_CHINA,
                L2235.Production_imports_GRID_CHINA,
                L2235.Production_elec_gen_GRID_CHINA)

  } else {
    stop("Unknown command")
  }
}
