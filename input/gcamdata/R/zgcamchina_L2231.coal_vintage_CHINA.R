# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_L2231.coal_vintage_CHINA
#'
#' Generates gcam-china model input for vintaging existing coal capacity.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2231.StubTechProd_coal_vintage_CHINA}, \code{L2231.StubTechEff_coal_vintage_CHINA}, \code{L2231.StubTechMarket_coal_vintage_CHINA},
#' \code{L2231.GlobalTechEff_coal_vintage_CHINA}, \code{L2231.GlobalTechCapFac_coal_vintage_CHINA}, \code{L2231.GlobalTechCapital_coal_vintage_CHINA}, \code{L2231.GlobalTechOMfixed_coal_vintage_CHINA},
#' \code{L2231.GlobalTechOMvar_coal_vintage_CHINA}, \code{L2231.GlobalTechShrwt_coal_vintage_CHINA}, \code{L2231.GlobalTechProfitShutdown_coal_vintage_CHINA},
#' \code{L2231.GlobalTechSCurve_coal_vintage_CHINA}.
#' @details This chunk adds vintaging for existing coal capacity, based on generation data, so that retirement is based on age of each group of powerplants.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author CW Dec 2025
#'


module_gcamchina_L2231.coal_vintage <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {

    return(c("L2234.StubTechProd_elecS_CHINA",
             "L2234.StubTechEff_elecS_CHINA",
             "L2234.StubTechMarket_elecS_CHINA",
             FILE = "gcam-china/MEIC2015_province_vint_gen",
             FILE = "gcam-china/province_names_mappings",
             "L2234.GlobalTechCapFac_elecS_CHINA",
             "L2234.GlobalTechCapital_elecS_CHINA",
             "L2234.GlobalTechEff_elecS_CHINA",
             "L2234.GlobalTechOMfixed_elecS_CHINA",
             "L2234.GlobalTechOMvar_elecS_CHINA",
             "L2234.GlobalTechProfitShutdown_elecS_CHINA",
             "L2234.GlobalTechSCurve_elecS_CHINA"))

  } else if(command == driver.DECLARE_OUTPUTS) {
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


  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    L2234.StubTechProd_elecS_CHINA <- get_data(all_data, "L2234.StubTechProd_elecS_CHINA", strip_attributes = TRUE)
    L2234.StubTechEff_elecS_CHINA <- get_data(all_data, "L2234.StubTechEff_elecS_CHINA", strip_attributes = TRUE)
    L2234.StubTechMarket_elecS_CHINA <- get_data(all_data, "L2234.StubTechMarket_elecS_CHINA", strip_attributes = TRUE)
    # L2234.GlobalTechShrwt_elecS_CHINA <- get_data(all_data, "L2234.GlobalTechShrwt_elecS_CHINA", strip_attributes = TRUE)
    L2234.GlobalTechCapFac_elecS_CHINA <- get_data(all_data, "L2234.GlobalTechCapFac_elecS_CHINA", strip_attributes = TRUE)
    L2234.GlobalTechCapital_elecS_CHINA <- get_data(all_data, "L2234.GlobalTechCapital_elecS_CHINA", strip_attributes = TRUE)
    L2234.GlobalTechOMfixed_elecS_CHINA <- get_data(all_data, "L2234.GlobalTechOMfixed_elecS_CHINA", strip_attributes = TRUE)
    L2234.GlobalTechOMvar_elecS_CHINA <- get_data(all_data, "L2234.GlobalTechOMvar_elecS_CHINA", strip_attributes = TRUE)
    L2234.GlobalTechEff_elecS_CHINA <- get_data(all_data, "L2234.GlobalTechEff_elecS_CHINA", strip_attributes = TRUE)
    L2234.GlobalTechProfitShutdown_elecS_CHINA <- get_data(all_data, "L2234.GlobalTechProfitShutdown_elecS_CHINA", strip_attributes = TRUE)
    L2234.GlobalTechSCurve_elecS_CHINA <- get_data(all_data, "L2234.GlobalTechSCurve_elecS_CHINA", strip_attributes = TRUE)

    gen_dist_prov <- get_data(all_data, "gcam-china/MEIC2015_province_vint_gen", strip_attributes = TRUE)
    # cap_dist_prov <- get_data(all_data, "gcam-china/MEIC2015_province_vint_cap", strip_attributes = TRUE)
    provNamesMapping <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = TRUE)


    gcamchina.COAL_VINTAGE_LABELS <- c("before 1990", "1991-1995","1996-2000", "2001-2005", "2006-2010", "2011-2015","2016-2021")
    gcamchina.AVG_COAL_PLANT_LIFETIME <- 40
    MODEL_BASE_YEARS <- c(1975,1990,2005,2010,2015,2021)
    gcamchina.COAL_RETIRE_STEEPNESS<- 0.3




gen_dist_prov %>%
  dplyr::rename("before 1990" = "install<=1990", "1991-1995" = "1990<install<=1995",
                "1996-2000" = "1995<install<=2000","2001-2005" = "2000<install<=2005",
                "2006-2010" = "2005<install<=2010","2011-2015" = "2010<install<=2015","2016-2021" = "2015<install<=2021") %>%
  dplyr::mutate(province.name = gsub("Ningxia Hui","Ningxia", province.name)) %>%
  dplyr::filter(province.name != "Hong Kong") %>%
  dplyr::left_join(provNamesMapping) %>%
  tidyr::gather(key = vintage.bin, value = generation, gcamchina.COAL_VINTAGE_LABELS) %>%
  dplyr::mutate(generation = ifelse(is.na(generation), 0, generation),
                Operating.Year = as.numeric(substr(vintage.bin, nchar(vintage.bin)-3, nchar(vintage.bin))),
                Retirement.Year = Operating.Year + gcamchina.AVG_COAL_PLANT_LIFETIME,
                lifetime = gcamchina.AVG_COAL_PLANT_LIFETIME - (max(MODEL_BASE_YEARS) - Operating.Year)) %>%
  dplyr::select("province", "vintage.bin", "generation", "lifetime", "Operating.Year") %>%
  group_by(province) %>%
  mutate(share.vintage = generation / sum(generation),
         share.vintage = ifelse(is.na(share.vintage), 1/(length(gcamchina.COAL_VINTAGE_LABELS)), share.vintage)) %>%
  ungroup() ->
  L2231.coal_vintage_gen_2021



# Apply vintage share by state to calibrated values and create table to be read in
L2231.coal_vintage_gen_2021 %>%
  rename(region = "province") %>%
  # LJENM is intended to duplicate rows so production can be allocated across vintages; use left_join to avoid error
  left_join(L2234.StubTechProd_elecS_CHINA %>%
              filter(subsector == "coal",
                     year == max(MODEL_BASE_YEARS)),
            by = "region") %>%
  filter(calOutputValue != 0) %>% ##, ) %>%   #removed to keep HK
  mutate(calOutputValue = calOutputValue * share.vintage,
         # Create new technologies. Naming the variable as stub.technology.new so that we can use stub.technology as reference later
         stub.technology.new = paste(stub.technology, vintage.bin, sep = " "),
         year = max(MODEL_BASE_YEARS), share.weight.year = max(MODEL_BASE_YEARS),
         subs.share.weight = 1, tech.share.weight = 1) %>%
  # Select variables. For now, include vintage.bin as well. We'll remove it later
  select(LEVEL2_DATA_NAMES[["StubTechProd"]], stub.technology.new, lifetime, vintage.bin, Operating.Year) ->
  L2231.StubTechProd_coal_vintage_CHINA


# Create a table to read in energy inputs and efficiencies for the new technologies in calibration years.
# Assuming equal efficiency across provinces and across vintages, but varying by historical year
# ##check if historically there is not much correlation between efficiency of a generator in China and its vintage.


L2231.StubTechProd_coal_vintage_CHINA %>%
  select(region, supplysector, subsector, year, stub.technology, stub.technology.new) %>%
  complete(nesting(region, supplysector, subsector, stub.technology, stub.technology.new), year = MODEL_BASE_YEARS)%>%
  left_join(L2234.StubTechEff_elecS_CHINA,
            by = c("region", "supplysector", "subsector", "stub.technology", "year"))%>%
  filter(complete.cases(.))%>%
  select(-market.name) %>%
  left_join(L2234.StubTechMarket_elecS_CHINA,
            by = c("region", "supplysector", "subsector", "stub.technology", "year", "minicam.energy.input")) %>% ##added to change market to provincial level from regional grid level
  filter(complete.cases(.)) %>%
  mutate(stub.technology = stub.technology.new) %>%
  select(LEVEL2_DATA_NAMES[["StubTechEff"]]) ->
  L2231.StubTechEff_coal_vintage_CHINA

# Changed markets to be based off of L223.StubTechMarket_elec_CHINA, rather than L2231.StubTechEff_coal_vintage_CHINA,
# so that the markets are at provincial level rather than electrical grid region level
L2231.StubTechProd_coal_vintage_CHINA %>%
  filter(year == max(MODEL_BASE_YEARS)) %>%
  select(LEVEL2_DATA_NAMES[["StubTechYr"]], stub.technology.new) %>%
  complete(nesting(region, supplysector, subsector, stub.technology, stub.technology.new), year = MODEL_FUTURE_YEARS)%>%
  filter(year != max(MODEL_BASE_YEARS)) %>%
  left_join_error_no_match(L2234.StubTechMarket_elecS_CHINA,
                           by = c("region", "supplysector", "subsector", "stub.technology", "year"))%>%
  mutate(stub.technology = stub.technology.new) %>%
  select(LEVEL2_DATA_NAMES[["StubTechMarket"]]) ->
  L2231.StubTechMarket_coal_vintage_CHINA



# # Read in energy inputs for future periods
# L2231.StubTechEff_coal_vintage_CHINA %>%
#   filter(year == max(MODEL_BASE_YEARS)) %>%
#   select(-efficiency, -year) %>%
#   repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) ->
#   L2231.StubTechMarket_coal_vintage_CHINA
#

# Create tables to read in energy and non-energy inputs for future years in global technology database
# Create a basic strucure with common variables

L2231.StubTechProd_coal_vintage_CHINA %>%
  select(supplysector, subsector, stub.technology, stub.technology.new, year, lifetime, Operating.Year) %>%
  unique() %>%
  complete(nesting(supplysector, subsector, stub.technology, stub.technology.new), year = MODEL_YEARS) %>%
  rename(sector.name = supplysector, subsector.name = subsector, technology = stub.technology.new) ->
  L2231.GlobalTech


# Energy inputs: Efficiency

L2231.GlobalTech %>%
  left_join(L2234.GlobalTechEff_elecS_CHINA %>%
              rename(sector.name = supplysector,
                     subsector.name = subsector),
            by = c("sector.name", "subsector.name","stub.technology" = "technology", "year")) %>%
  select(LEVEL2_DATA_NAMES[["GlobalTechEff"]]) ->
  L2231.GlobalTechEff_coal_vintage_CHINA


# Non-energy inputs: capacity factor, capital costs, fixed and variable OM costs
# Capacity factor:
L2231.GlobalTech %>%
  left_join(L2234.GlobalTechCapFac_elecS_CHINA %>%
              rename(sector.name = supplysector,
                     subsector.name = subsector),
            by = c("sector.name", "subsector.name","stub.technology" = "technology", "year")) %>%
  select(LEVEL2_DATA_NAMES[["GlobalTechCapFac"]]) ->
  L2231.GlobalTechCapFac_coal_vintage_CHINA


# Capital costs:
L2231.GlobalTech %>%
  left_join(L2234.GlobalTechCapital_elecS_CHINA %>%
              rename(sector.name = supplysector,
                     subsector.name = subsector),
            by = c("sector.name", "subsector.name", "stub.technology" = "technology", "year")) %>%
  select(LEVEL2_DATA_NAMES[["GlobalTechCapital"]]) ->
  L2231.GlobalTechCapital_coal_vintage_CHINA

# Fixed OM costs:
L2231.GlobalTech %>%
  left_join(L2234.GlobalTechOMfixed_elecS_CHINA %>%
              rename(sector.name = supplysector,
                     subsector.name = subsector),
            by = c("sector.name", "subsector.name", "stub.technology" = "technology", "year")) %>%
  select(LEVEL2_DATA_NAMES[["GlobalTechOMfixed"]]) ->
  L2231.GlobalTechOMfixed_coal_vintage_CHINA

# Variable OM costs
L2231.GlobalTech %>%
  left_join_error_no_match(L2234.GlobalTechOMvar_elecS_CHINA %>%
                             rename(sector.name = supplysector,
                                    subsector.name = subsector),
                           by = c("sector.name", "subsector.name", "stub.technology" = "technology", "year")) %>%
  select(LEVEL2_DATA_NAMES[["GlobalTechOMvar"]]) ->
  L2231.GlobalTechOMvar_coal_vintage_CHINA

# Create table to read in shareweights in future years in global technology database
L2231.GlobalTech %>%
  mutate(share.weight = 0) %>%
  select(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]]) ->
  L2231.GlobalTechShrwt_coal_vintage_CHINA


# Profit Shutdown decider
# L2231.GlobalTechProfitShutdown_coal_vintage_CHINA: Profit shut-down decider for historic China conventional coal electricity plants
L2231.GlobalTech %>%
  filter(year >= max(MODEL_BASE_YEARS)) %>%
  left_join(L2234.GlobalTechProfitShutdown_elecS_CHINA %>%
              rename(sector.name = supplysector,
                     subsector.name = subsector),
            by = c("sector.name", "subsector.name", "stub.technology" = "technology", "year")) %>%
  select(LEVEL2_DATA_NAMES[["GlobalTechProfitShutdown"]]) ->
  L2231.GlobalTechProfitShutdown_coal_vintage_CHINA


vintage_timestep <- (unique(L2231.GlobalTech$Operating.Year)[order(unique(L2231.GlobalTech$Operating.Year)) == 4]-unique(L2231.GlobalTech$Operating.Year)[order(unique(L2231.GlobalTech$Operating.Year)) == 3])

# GlobalTechScurve ===============
# Create a table to read in S-curve parameters for vintage bin techs by state
# Making the oldest vintage bin have a half life 10 years before the lifetime of that bin,
# rather than 5 years before, as is the case for the other vintage bins
L2231.GlobalTech %>%
  filter(year == max(MODEL_BASE_YEARS)) %>%
  left_join(L2234.GlobalTechSCurve_elecS_CHINA %>%
              rename(sector.name = supplysector,
                     subsector.name = subsector)%>%
              select(-lifetime),
            by = c("sector.name", "subsector.name", "stub.technology" = "technology", "year")) %>%
  mutate(half.life = ifelse(Operating.Year == min(unique(L2231.GlobalTech$Operating.Year), na.rm = T),
                            lifetime - 2*vintage_timestep,
                            lifetime - vintage_timestep),
         # half life has to at minimum be one time period past calibration year
         lifetime = ifelse(half.life <= 0, vintage_timestep + vintage_timestep, lifetime),
         half.life = ifelse(half.life <= 0, vintage_timestep, half.life),
         steepness = gcamchina.COAL_RETIRE_STEEPNESS) %>%
  select(sector.name, subsector.name, technology,year, lifetime, steepness, half.life) ->
  L2231.GlobalTechSCurve_coal_vintage_CHINA

# Clean up StubTechProd table
L2231.StubTechProd_coal_vintage_CHINA %>%
  mutate(stub.technology = stub.technology.new) %>%
  select(LEVEL2_DATA_NAMES[["StubTechProd"]]) %>%
  complete(nesting(region, supplysector, subsector, stub.technology), year = MODEL_BASE_YEARS) %>%
  mutate(share.weight.year = year) %>%
  # Read in zero caloutputvalue for other base years
  replace_na(list(calOutputValue = 0, subs.share.weight = 1, tech.share.weight = 0)) ->
  L2231.StubTechProd_coal_vintage_CHINA


# Read in zero calOutputValue for 2015 for existing coal conv pul technology
L2234.StubTechProd_elecS_CHINA %>%
  filter(subsector == "coal",
         year == max(MODEL_BASE_YEARS),
         calOutputValue != 0,
         !grepl("_retire_", stub.technology),
         region %in% unique(L2231.coal_vintage_gen_2021$province)) %>%
  mutate(calOutputValue = 0, tech.share.weight = 0) %>%
  select(LEVEL2_DATA_NAMES[["StubTechProd"]]) %>%
  bind_rows(L2231.StubTechProd_coal_vintage_CHINA) %>%
  arrange(region, year) ->
  L2231.StubTechProd_coal_vintage_CHINA

# ===================================================
# Produce outputs

L2231.StubTechProd_coal_vintage_CHINA %>%
  add_title("Calibration outputs for conventional coal electricity plants by detailed vintage and province") %>%
  add_units("EJ") %>%
  add_comments("Generation shares by vintage are calculated based on MEIC generation data from 2015") %>%
  add_comments("Generation shares by vintage are then applied to stub-technology 2021 generation in each state") %>%
  add_comments("Generation in other base years are set to zero to each vintage stub-technology") %>%
  add_legacy_name("L2231.StubTechProd_coal_vintage_CHINA") %>%
  add_precursors(
    "gcam-china/MEIC2015_province_vint_gen", ##make sure using gen not cap
    "gcam-china/province_names_mappings",
    "L2234.StubTechProd_elecS_CHINA") ->
  L2231.StubTechProd_coal_vintage_CHINA

L2231.StubTechEff_coal_vintage_CHINA %>%
  add_title("Efficiencies of conventional coal electricity plants by detailed vintage and province in calibration years") %>%
  add_units("Unitless") %>%
  add_comments("Apply the same efficiencies to all vintage groups") %>%
  add_legacy_name("L2231.StubTechEff_coal_vintage_CHINA") %>%
  same_precursors_as("L2231.StubTechProd_coal_vintage_CHINA") %>%
  add_precursors("L2234.StubTechEff_elecS_CHINA",
                 "L2234.StubTechMarket_elecS_CHINA") ->
  L2231.StubTechEff_coal_vintage_CHINA

L2231.StubTechMarket_coal_vintage_CHINA %>%
  add_title("Energy inputs for conventional coal electricity plants by detailed vintage and state") %>%
  add_units("Unitless") %>%
  add_comments("Same to all vintage groups") %>%
  add_legacy_name("L2231.StubTechMarket_coal_vintage_CHINA") %>%
  same_precursors_as("L2231.StubTechEff_coal_vintage_CHINA") ->
  L2231.StubTechMarket_coal_vintage_CHINA


L2231.GlobalTechEff_coal_vintage_CHINA %>%
  add_title("Efficiencies for conventional coal electricity plants by detailed vintage") %>%
  add_units("Unitless") %>%
  add_comments("Same to all vintage groups") %>%
  add_legacy_name("L2231.GlobalTechEff_coal_vintage_CHINA") %>%
  same_precursors_as("L2231.StubTechProd_coal_vintage_CHINA") %>%
  add_precursors("L2234.GlobalTechEff_elecS_CHINA") ->
  L2231.GlobalTechEff_coal_vintage_CHINA

L2231.GlobalTechShrwt_coal_vintage_CHINA %>%
  add_title("Shareweights for conventional coal electricity plants by detailed vintage") %>%
  add_units("Unitless") %>%
  add_comments("Set zero shareweights for all vintage stub-technologies") %>%
  add_legacy_name("L2231.GlobalTechShrwt_coal_vintage_CHINA") %>%
  same_precursors_as("L2231.GlobalTechEff_coal_vintage_CHINA") ->
  L2231.GlobalTechShrwt_coal_vintage_CHINA

L2231.GlobalTechCapFac_coal_vintage_CHINA %>%
  add_title("Capacity factors for conventional coal electricity plants by detailed vintage") %>%
  add_units("Unitless") %>%
  add_comments("Same to all vintage groups") %>%
  add_legacy_name("L2231.GlobalTechCapFac_coal_vintage_CHINA") %>%
  same_precursors_as("L2231.GlobalTechEff_coal_vintage_CHINA") %>%
  add_precursors("L2234.GlobalTechCapFac_elecS_CHINA") ->
  L2231.GlobalTechCapFac_coal_vintage_CHINA

L2231.GlobalTechCapital_coal_vintage_CHINA %>%
  add_title("Capital costs for conventional coal electricity plants by detailed vintage") %>%
  add_units("1975$US per kW; unitless (fixed.charge.rate)") %>%
  add_comments("Same to all vintage groups") %>%
  add_legacy_name("L2231.GlobalTechCapital_coal_vintage_CHINA") %>%
  same_precursors_as("L2231.GlobalTechEff_coal_vintage_CHINA") %>%
  add_precursors("L2234.GlobalTechCapital_elecS_CHINA") ->
  L2231.GlobalTechCapital_coal_vintage_CHINA

L2231.GlobalTechOMfixed_coal_vintage_CHINA %>%
  add_title("Fixed OM costs for conventional coal electricity plants by detailed vintage") %>%
  add_units("1975$/kW/yr") %>%
  add_comments("Same to all vintage groups") %>%
  add_legacy_name("L2231.GlobalTechOMfixed_coal_vintage_CHINA") %>%
  same_precursors_as("L2231.GlobalTechEff_coal_vintage_CHINA") %>%
  add_precursors("L2234.GlobalTechOMfixed_elecS_CHINA") ->
  L2231.GlobalTechOMfixed_coal_vintage_CHINA

L2231.GlobalTechOMvar_coal_vintage_CHINA %>%
  add_title("Variable OM costs for conventional coal electricity plants by detailed vintage") %>%
  add_units("1975$/MWh") %>%
  add_comments("Same to all vintage groups") %>%
  add_legacy_name("L2231.GlobalTechOMvar_coal_vintage_CHINA") %>%
  same_precursors_as("L2231.GlobalTechEff_coal_vintage_CHINA") %>%
  add_precursors("L2234.GlobalTechOMvar_elecS_CHINA") ->
  L2231.GlobalTechOMvar_coal_vintage_CHINA

L2231.GlobalTechProfitShutdown_coal_vintage_CHINA %>%
  add_title("Profit shutdown parameters for conventional coal electricity plants by detailed vintage") %>%
  add_units("Unitless") %>%
  add_comments("Same to all vintage groups") %>%
  add_legacy_name("L2231.GlobalTechProfitShutdown_coal_vintage_CHINA") %>%
  same_precursors_as("L2231.GlobalTechEff_coal_vintage_CHINA") %>%
  add_precursors("L2234.GlobalTechProfitShutdown_elecS_CHINA") ->
  L2231.GlobalTechProfitShutdown_coal_vintage_CHINA

L2231.GlobalTechSCurve_coal_vintage_CHINA %>%
  add_title("S Curve parameters for conventional coal electricity plants by detailed vintage") %>%
  add_units("Unitless") %>%  ##check this
  add_comments("Same to all vintage groups") %>%
  add_legacy_name("L2231.GlobalTechSCurve_coal_vintage_CHINA") %>%
  same_precursors_as("L2231.GlobalTechEff_coal_vintage_CHINA") %>%
  add_precursors("L2234.GlobalTechSCurve_elecS_CHINA") ->
  L2231.GlobalTechSCurve_coal_vintage_CHINA

return_data(
  L2231.StubTechProd_coal_vintage_CHINA,
  L2231.StubTechEff_coal_vintage_CHINA,
  L2231.StubTechMarket_coal_vintage_CHINA,
  L2231.GlobalTechEff_coal_vintage_CHINA,
  L2231.GlobalTechCapFac_coal_vintage_CHINA,
  L2231.GlobalTechCapital_coal_vintage_CHINA,
  L2231.GlobalTechOMfixed_coal_vintage_CHINA,
  L2231.GlobalTechOMvar_coal_vintage_CHINA,
  L2231.GlobalTechShrwt_coal_vintage_CHINA,
  L2231.GlobalTechProfitShutdown_coal_vintage_CHINA,
  L2231.GlobalTechSCurve_coal_vintage_CHINA)

  } else {
    stop("Unknown command")
  }
}
