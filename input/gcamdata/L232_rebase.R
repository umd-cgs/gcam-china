library(gcamdata)
source("./R/constants.R")
source("./R/module-helpers.R")
source("./R/pipeline-helpers.R")
source("./R/utils-data.R")
source("./rebase/generate_level2.R")
library(dplyr)
library(tidyr)
library(tibble)
library(assertthat)
library(magrittr)
library(readr)

# import data
GCAM_region_names <- read_csv("./inst/extdata/common/GCAM_region_names.csv", comment = "#")
calibrated_techs <- read_csv("./inst/extdata/energy/calibrated_techs.csv", comment = "#")
A_regions <- read_csv("./inst/extdata/energy/A_regions.csv", comment = "#")
calibrated_techs <- read_csv("./inst/extdata/energy/calibrated_techs.csv", comment = "#")
A23.elecS_tech_mapping <- read_csv("./inst/extdata/gcam-china/A23.elecS_tech_mapping.csv", comment = "#")
A23.elecS_tech_availability <- read_csv("./inst/extdata/gcam-china/A23.elecS_tech_availability.csv", comment = "#")
A24.sector <- read_csv("./inst/extdata/energy/A24.sector.csv", comment = "#")
A24.subsector_logit <- read_csv("./inst/extdata/energy/A24.subsector_logit.csv", comment = "#")
A24.subsector_shrwt <- read_csv("./inst/extdata/energy/A24.subsector_shrwt.csv", comment = "#")
A24.subsector_interp <- read_csv("./inst/extdata/energy/A24.subsector_interp.csv", comment = "#")
A24.globaltech_shrwt <- read_csv("./inst/extdata/energy/A24.globaltech_shrwt.csv", comment = "#")
L1231.out_EJ_province_elec_F_tech <- read_csv("./outputs/L1231.out_EJ_province_elec_F_tech.csv", comment = "#")
L124.in_EJ_R_heat_F_Yh <- read_csv("./outputs/L124.in_EJ_R_heat_F_Yh.csv", comment = "#")
L124.out_EJ_R_heatfromelec_F_Yh <- read_csv("./outputs/L124.out_EJ_R_heatfromelec_F_Yh.csv", comment = "#")
L132.in_EJ_province_indnochp_F <- read_csv("./outputs/L132.in_EJ_province_indnochp_F.csv", comment = "#")
L144.in_EJ_province_bld_F_U <- read_csv("./outputs/L144.in_EJ_province_bld_F_U.csv", comment = "#")
L1231.eff_R_elec_F_tech_Yh <- read_csv("./outputs/L1231.eff_R_elec_F_tech_Yh.csv", comment = "#")

L2234.StubTechProd_elecS_CHINA <- read_csv("./outputs/L2234.StubTechProd_elecS_CHINA.csv", comment = "#")


# ===================================================
# Data Processing

A24.globaltech_shrwt %>%
  gather_years(value_col = "share.weight") -> A24.globaltech_shrwt



#Sum heat consumption in industry and heat sector, scale the heat supply
L132.in_EJ_province_indnochp_F %>%
  filter(fuel == "heat") %>%
  select(province,fuel,year,value) %>%
  left_join(L144.in_EJ_province_bld_F_U %>%
              filter(service == "Heating") %>%
              select(province, fuel, year, value) %>%
              group_by(province, fuel, year) %>%
              summarise(value = sum(value)) %>%
              ungroup, by = c("province", "fuel", "year")) %>%
  replace_na(list(value.y = 0)) %>%
  mutate(value = value.x + value.y) %>%
  group_by(fuel, year) %>%
  mutate(multiplier = replace_na(value / sum(value, na.rm = T),0)) %>%
  ungroup() %>%
  select(province, year, multiplier) ->
  heat_en_by_province

# ===================================================
# Write to all provinces
# Supply sector information for district heat sectors
A24.sector %>%
  mutate(region = "China") %>%
  write_to_all_provinces(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), gcamchina.PROVINCES_NOHKMC) ->
  L224.Supplysector_heat_china

# delete heat sector in the CHINA region
L224.Supplysector_heat_china %>%
  filter(region == "AH") %>%
  mutate(region = "China") %>%
  select(LEVEL2_DATA_NAMES[["DeleteSupplysector"]]) ->
  L224.DeleteSupplysector_CHINAheat  ## OUTPUT

# Subsector logit exponents of district heat sectors
A24.subsector_logit %>%
  write_to_all_provinces(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), gcamchina.PROVINCES_NOHKMC) ->
  L224.SubsectorLogit_heat_china

# L224.SubsectorShrwt_heat_china and L224.SubsectorShrwtFllt_heat_china: Subsector shareweights of district heat sectors
if(any(!is.na(A24.subsector_shrwt$year))) {
  A24.subsector_shrwt %>%
    filter(!is.na(year)) %>%
    write_to_all_provinces(LEVEL2_DATA_NAMES[["SubsectorShrwt"]], gcamchina.PROVINCES_NOHKMC ) ->
    L224.SubsectorShrwt_heat_china
}

if(any(!is.na(A24.subsector_shrwt$year.fillout))) {
  A24.subsector_shrwt %>%
    filter(!is.na(year.fillout)) %>%
    write_to_all_provinces(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], gcamchina.PROVINCES_NOHKMC ) ->
    L224.SubsectorShrwtFllt_heat_china
}

# scale heat to all provinces
L124.in_EJ_R_heat_F_Yh %>%
  left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
  left_join(calibrated_techs %>%
              select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
              distinct, by = c("sector", "fuel")) %>%
  rename(stub.technology = technology) %>%
  filter(year %in% MODEL_BASE_YEARS) %>%
  filter(region == "China") %>%
  select(LEVEL2_DATA_NAMES[["StubTechYr"]], "minicam.energy.input", "value") %>%
  mutate(calibrated.value = round(value, energy.DIGITS_CALOUTPUT),
         share.weight.year = year,
         subs.share.weight = if_else(calibrated.value == 0, 0, 1),
         tech.share.weight = subs.share.weight) %>%
  select(-value) %>%
  write_to_all_provinces(LEVEL2_DATA_NAMES[["StubTechCalInput"]], gcamchina.PROVINCES_NOHKMC ) %>%
  # use province heat consumption to scale
  left_join(heat_en_by_province %>% rename(region = province),by = c("region","year")) %>%
  mutate(calibrated.value = replace_na(calibrated.value * multiplier,0)) %>%
  select(-multiplier) ->
  L224.StubTechCalInput_heat_china

# cal heat produced by CHP by province
L124.out_EJ_R_heatfromelec_F_Yh %>%
  filter(GCAM_region_ID == 11) %>%
  write_to_all_provinces(c('region','fuel','sector','year','value'), gcamchina.PROVINCES_NOHKMC ) %>%
  left_join(heat_en_by_province %>% rename(region = province) ,by = c("region","year")) %>%
  mutate(value = replace_na(value * multiplier,0)) %>%
  select(-multiplier) ->
  L124.out_EJ_province_heatfromelec_F_Yh

no_gas_tech_2010 <- L2234.StubTechProd_elecS_CHINA %>%
  filter(year == 2010 & subsector == "gas") %>%
  filter(grepl("steam/CT", stub.technology)) %>%
  group_by(region, year) %>%
  summarise(no_gas = sum(calOutputValue)) %>%
  ungroup() %>%
  filter(no_gas == 0)

#Just move gas CHP to coal, because some region do not have gas generation
L124.out_EJ_province_heatfromelec_F_Yh %>%
  # mutate(fuel_new = if_else((region %in% c("NX","FJ","HN","JX") & (fuel == "gas")),"coal",fuel)) %>%
  # mutate(fuel = fuel_new) %>%
  # select(-fuel_new) %>%
  mutate(fuel_new = if_else((region %in% c(no_gas_tech_2010$region) & year == 2010 & (fuel == "gas")),"coal",fuel)) %>%
  mutate(fuel = fuel_new) %>%
  group_by(region, fuel, sector, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  complete(nesting(region, year, sector), fuel = c('biomass','gas','coal','refined liquids')) %>%
  mutate(value = replace_na(value,0)) ->
  L124.out_EJ_province_heatfromelec_F_Yh

L2234.StubTechProd_elecS_CHINA %>%
  filter(year %in% HISTORICAL_YEARS) %>%
  left_join(A23.elecS_tech_mapping %>%
              select(Electric.sector.technology, technology) %>%
              distinct(), by = c("stub.technology" = "Electric.sector.technology")) %>%
  # drop renewables (wind, solar, CSP)
  na.omit() %>%
  filter(technology %in% calibrated_techs$technology[calibrated_techs$secondary.output == "heat"]) %>%
  mutate(province = region, sector = "electricity generation", fuel = subsector) %>%
  group_by(province, sector, fuel, technology, year) %>%
  summarise(value = sum(calOutputValue), .groups = "drop") %>%
  ungroup() %>%
  left_join_error_no_match(L124.out_EJ_province_heatfromelec_F_Yh %>%
                             rename(value_heatfromelec = value,province = region) %>%
                             rename(temp = sector), by = c("province", "fuel", "year")) %>%
  # Heat output divided by electricity output
  mutate(value = value_heatfromelec / value) %>%
  select(province, sector, fuel, technology, year, value) %>%
  # Reset missing and infinite values (applicable for CC in the base years) to 0
  mutate(value = if_else(is.na(value) | is.infinite(value), 0, value)) ->
  L124.heatoutratio_province_elec_F_tech_Yh

# Drop all rows where value = 0 for all years
# Create a table of all the years of all value = 0
L124.heatoutratio_province_elec_F_tech_Yh %>%
  group_by(province, technology, sector, fuel) %>%
  summarise(sum = sum(value)) %>%
  ungroup() %>%
  filter(sum == 0) -> years_heatout_0

# Filter out the years in years_heatout_0 from heatoutratio
L124.heatoutratio_province_elec_F_tech_Yh %>%
  left_join(years_heatout_0,
            by = c("province", "sector", "fuel", "technology")) %>%
  # Using 1 for all rows where heatout is not 0 for all years
  replace_na(list(sum = 1)) %>%
  filter(sum != 0) %>%
  select(-sum) -> L124.heatoutratio_province_elec_F_tech_Yh

# Subsector shareweight interpolation of district heat sectors
if(any(is.na(A24.subsector_interp$to.value))) {
  A24.subsector_interp %>%
    filter(is.na(to.value)) %>%
    write_to_all_provinces(LEVEL2_DATA_NAMES[["SubsectorInterp"]], gcamchina.PROVINCES_NOHKMC ) ->
    L224.SubsectorInterp_heat_china
}

if(any(!is.na(A24.subsector_interp$to.value))) {
  A24.subsector_interp %>%
    filter(!is.na(to.value)) %>%
    write_to_all_provinces(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], gcamchina.PROVINCES_NOHKMC ) ->
    L224.SubsectorInterpTo_heat_china
}

# Identification of stub technologies of district heat
# Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
A24.globaltech_shrwt %>%
  select(supplysector, subsector, technology) %>%
  distinct %>%
  write_to_all_provinces(LEVEL2_DATA_NAMES[["Tech"]], gcamchina.PROVINCES_NOHKMC) %>%
  rename(stub.technology = technology)  ->
  L224.StubTech_heat_china

# Secondary output of heat, applied to electricity generation technologies
# NOTE: This is complicated. Initially tried using historical information for all model periods that fall within historical time
# (i.e. not just the model base years). However for regions like the FSU where historical periods often have very low output of heat
# from the district heat sector, and most heat as a secondary output from the electricity sector, the secondary output heat can easily
# exceed the demands from the end-use sectors, causing model solution failure. For this reason, the convention applied here is to
# use the secondary output of heat from the power sector only in the model base years.
L124.heatoutratio_province_elec_F_tech_Yh %>%
  rename(region = province) %>%
  filter(year %in% MODEL_BASE_YEARS) %>%
  select(region, technology, year, value) %>%
  # Yang Ou update: map to electricity segments
  # use left_join because here need to copy to all segments
  left_join(A23.elecS_tech_mapping %>%
              select(Electric.sector, subsector, Electric.sector.technology, technology) %>%
              distinct(), by = c("technology")) %>%
  anti_join(A23.elecS_tech_availability,
            by = c("Electric.sector.technology" = "stub.technology")) %>%
  mutate(supplysector = Electric.sector,
         stub.technology = Electric.sector.technology,
         secondary.output = A24.sector[["supplysector"]]) %>%
  select(LEVEL2_DATA_NAMES[["StubTechYr"]], "secondary.output", "value") %>%
  mutate(output.ratio = round(value, energy.DIGITS_CALOUTPUT)) %>%
  select(-value) -> L224.StubTechSecOut_elec_china

# Calculate cost adjustment, equal to the output of heat multiplied by the heat price
# (to minimize the distortion of including the secondary output)
L224.StubTechSecOut_elec_china %>%
  select(LEVEL2_DATA_NAMES[["StubTechYr"]], "output.ratio") %>%
  mutate(minicam.non.energy.input = "heat plant",
         input.cost = round(output.ratio*energy.HEAT_PRICE, energy.DIGITS_COST))-> L224.StubTechCost_elec

# The secondary output of heat from CHP in the electric sector can cause the price of the technologies
# to go very low or negative if the technology cost is not modified to reflect the additional costs of
# CHP systems (as compared with electricity-only systems). Low technology costs can cause unrealistically
# low electricity prices in the calibration year, distorting behavior in future years. In this method,
# costs related to heat production and distribution are backed out from exogenous heat prices and data-derived heat:power ratios.
L1231.eff_R_elec_F_tech_Yh %>%
  filter(year %in% MODEL_YEARS) %>%
  rename(efficiency = value) %>%
  filter(GCAM_region_ID == 11) %>%
  write_to_all_provinces(c('region','sector','fuel','technology','year','efficiency'), gcamchina.PROVINCES_NOHKMC) %>%
  filter(fuel == "gas" | fuel == "coal") %>%
  filter(efficiency < energy.DEFAULT_ELECTRIC_EFFICIENCY) %>%
  mutate(cost_modifier = energy.GAS_PRICE * (1 / energy.DEFAULT_ELECTRIC_EFFICIENCY - 1 / efficiency)) ->
  L224.eff_cost_adj_Rh_elec_gas_sc_Y

# Modify the costs
L224.StubTechCost_elec %>%
  left_join(L224.eff_cost_adj_Rh_elec_gas_sc_Y %>%
              rename(subsector = fuel, stub.technology = technology) %>%
              select(region, subsector, stub.technology, year, cost_modifier),
            by = c("region", "subsector", "stub.technology", "year")) %>%
  mutate(input.cost = if_else(!is.na(cost_modifier), round(pmax(0, input.cost + cost_modifier), energy.DIGITS_COST), input.cost)) %>%
  select(-cost_modifier, -output.ratio) -> L224.StubTechCost_elec

# Need to fill out object names for all model time periods
L224.StubTechCost_elec %>%
  filter(year == max(year)) %>%
  select(-year) %>%
  repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
  mutate(input.cost = 0) -> L224.StubTechCost_elec_fut

L224.StubTechCost_elec %>%
  bind_rows(L224.StubTechCost_elec_fut) -> L224.StubTechCost_elec_china

# Get markets for fuels consumed by the province heat sectors
L224.StubTech_heat_china %>%
  repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
  left_join_keep_first_only(L224.StubTechCalInput_heat_china %>%
                              select(supplysector, subsector, stub.technology, minicam.energy.input),
                            by = c("supplysector", "subsector", "stub.technology")) %>%
  filter(is.na(minicam.energy.input) == FALSE) %>%
  mutate(market.name = region) %>%
  select(LEVEL2_DATA_NAMES[["StubTechMarket"]]) %>%
  mutate(market.name = if_else(minicam.energy.input == 'delivered biomass',
                               'China', market.name)) ->
  L224.StubTechMarket_heat_CHINA  ## OUTPUT
# ===================================================

# Produce outputs

L224.Supplysector_heat_china %>%
  add_title("Supply sector information for district heat sectors") %>%
  add_units("N/A") %>%
  add_comments("Supply sector info for district heat is written to all regions, ") %>%
  add_comments("filtered by which regions have district heat") %>%
  add_legacy_name("L224.Supplysector_heat_china") %>%
  add_precursors("energy/A24.sector", "energy/A_regions", "common/GCAM_region_names") ->
  L224.Supplysector_heat_china

L224.SubsectorLogit_heat_china %>%
  add_title("Subsector logit exponents of district heat sectors") %>%
  add_units("N/A") %>%
  add_comments("Subsector logit exponents for district heat is written to all regions, ") %>%
  add_comments("filtered by which regions have district heat") %>%
  add_legacy_name("L224.SubsectorLogit_heat_china") %>%
  add_precursors("energy/A24.subsector_logit", "energy/A_regions", "common/GCAM_region_names") ->
  L224.SubsectorLogit_heat_china

if(exists("L224.SubsectorShrwt_heat_china")) {
  L224.SubsectorShrwt_heat_china %>%
    add_title("Subsector shareweights of district heat sectors") %>%
    add_units("N/A") %>%
    add_comments("If year is not NA: Subsector shareweights for district heat written to all regions, ") %>%
    add_comments("filtered by which regions have district heat") %>%
    add_legacy_name("L224.SubsectorShrwt_heat_china") %>%
    add_precursors("energy/A24.subsector_shrwt", "energy/A_regions", "common/GCAM_region_names") ->
    L224.SubsectorShrwt_heat_china
} else {
  # If year column of A24.subsector_shrwt is all N/A, then a blank tibble is produced
  # (and presumably the following tibble, using year.fillout is made)
  missing_data() %>%
    add_legacy_name("L224.SubsectorShrwt_heat_china") ->
    L224.SubsectorShrwt_heat_china
}

if(exists("L224.SubsectorShrwtFllt_heat_china")) {
  L224.SubsectorShrwtFllt_heat_china %>%
    add_title("Subsector shareweights of district heat sectors with fillout year") %>%
    add_units("N/A") %>%
    add_comments("If year.fillout is not NA: Subsector shareweights for district heat written to all regions, ") %>%
    add_comments("filtered by which regions have district heat, uses year.fillout") %>%
    add_legacy_name("L224.SubsectorShrwtFllt_heat_china") %>%
    add_precursors("energy/A24.subsector_shrwt", "energy/A_regions", "common/GCAM_region_names") ->
    L224.SubsectorShrwtFllt_heat_china
} else {
  # If year.fillout column of A24.subsector_shrwt is all N/A, then a blank tibble is produced
  missing_data() %>%
    add_legacy_name("L224.SubsectorShrwtFllt_heat_china") ->
    L224.SubsectorShrwtFllt_heat_china
}

if(exists("L224.SubsectorInterp_heat_china")) {
  L224.SubsectorInterp_heat_china %>%
    add_title("Subsector shareweight interpolation of district heat sectors") %>%
    add_units("units") %>%
    add_comments("Interpolated data from A24.subsector_interp, ") %>%
    add_comments("filtered by which regions have district heat") %>%
    add_legacy_name("L224.SubsectorInterp_heat_china") %>%
    add_precursors("energy/A24.subsector_interp", "energy/A_regions", "common/GCAM_region_names") ->
    L224.SubsectorInterp_heat_china
} else {
  # If interp.to column of A24.subsector_interp contains no N/A values, then a blank tibble is produced
  missing_data() %>%
    add_legacy_name("L224.SubsectorInterp_heat_china") ->
    L224.SubsectorInterp_heat_china
}

if(exists("L224.SubsectorInterpTo_heat_china")) {
  L224.SubsectorInterpTo_heat_china %>%
    add_title("Subsector shareweight interpolation of district heat sectors using to.year") %>%
    add_units("units") %>%
    add_comments("Interpolated data from A24.subsector_interp, ") %>%
    add_comments("filtered by which regions have district heat") %>%
    add_legacy_name("L224.SubsectorInterpTo_heat_china") %>%
    add_precursors("energy/A24.subsector_interp", "energy/A_regions", "common/GCAM_region_names") ->
    L224.SubsectorInterpTo_heat_china
} else {
  # If interp.to column of A24.subsector_interp contains N/A values, then a blank tibble is produced
  missing_data() %>%
    add_legacy_name("L224.SubsectorInterpTo_heat_china") ->
    L224.SubsectorInterpTo_heat_china
}

L224.StubTech_heat_china %>%
  add_title("Identification of stub technologies of district heat") %>%
  add_units("N/A") %>%
  add_comments("A24.globaltech_shrwt written to all regions, filtered regions with district heat") %>%
  add_legacy_name("L224.StubTech_heat_china") %>%
  add_precursors("energy/A24.globaltech_shrwt", "energy/A_regions", "common/GCAM_region_names") ->
  L224.StubTech_heat_china

L224.StubTechCalInput_heat_china %>%
  add_title("Calibrated input to district heat") %>%
  add_units("EJ/yr") %>%
  add_comments("L124.in_EJ_R_heat_F_Yh and calibrated_techs are joined, shareweights assigned") %>%
  add_comments("as 0 if the calibrated value is 0 and 1 if it is not 0") %>%
  add_legacy_name("L224.StubTechCalInput_heat_china") %>%
  add_precursors("L124.in_EJ_R_heat_F_Yh",
                 "L144.in_EJ_province_bld_F_U",
                 "L132.in_EJ_province_indnochp_F",
                 "energy/calibrated_techs",
                 "energy/A_regions",
                 "common/GCAM_region_names") ->
  L224.StubTechCalInput_heat_china

L224.StubTechSecOut_elec_china %>%
  add_title("Secondary output of district heat from electricity technologies") %>%
  add_units("EJ") %>%
  add_comments("L124.heatoutratio_R_elec_F_tech_Yh used to determine secondary output heat from elec, ") %>%
  add_comments("filtering for only model base years") %>%
  add_legacy_name("L224.StubTechSecOut_elec_china") %>%
  add_precursors("L1231.out_EJ_province_elec_F_tech",
                 "L2234.StubTechProd_elecS_CHINA",
                 "L124.out_EJ_R_heatfromelec_F_Yh",
                 "gcam-china/A23.elecS_tech_mapping",
                 "gcam-china/A23.elecS_tech_availability",
                 "energy/A24.sector") ->
  L224.StubTechSecOut_elec_china

L224.StubTechCost_elec_china %>%
  add_title("Stubtech costs with secondary output heat") %>%
  add_units("1975$/GJ") %>%
  add_comments("From L224.StubTechSecOut_elec calculate cost adjustment, equal to the output of heat multiplied by the heat price") %>%
  add_comments("modify costs for technologies with efficiencies below default, apply to all model periods") %>%
  add_legacy_name("L224.StubTechCost_elec_china") %>%
  add_precursors("L1231.out_EJ_province_elec_F_tech",
                 "L1231.eff_R_elec_F_tech_Yh",
                 "gcam-china/A23.elecS_tech_mapping",
                 "gcam-china/A23.elecS_tech_availability",
                 "energy/A24.sector",
                 "energy/A_regions") ->
  L224.StubTechCost_elec_china

L224.DeleteSupplysector_CHINAheat %>%
  add_title("CHINA heat supply sectors") %>%
  add_units("EJ") %>%
  add_comments("Delete heat sector") %>%
  add_legacy_name("L224.DeleteSupplysector_CHINAheat") %>%
  add_precursors("energy/A24.sector") ->
  L224.DeleteSupplysector_CHINAheat

L224.StubTechMarket_heat_CHINA %>%
  add_title("CHINA heat supply sectors") %>%
  add_units("N/A") %>%
  add_comments("Market in heat sector") %>%
  add_legacy_name("L224.StubTechMarket_heat_CHINA") %>%
  add_precursors("energy/A24.sector") ->
  L224.StubTechMarket_heat_CHINA

return_data(L224.Supplysector_heat_china,
            L224.SubsectorLogit_heat_china,
            L224.SubsectorShrwt_heat_china,
            L224.SubsectorShrwtFllt_heat_china,
            L224.SubsectorInterp_heat_china,
            L224.SubsectorInterpTo_heat_china,
            L224.StubTech_heat_china,
            L224.StubTechCalInput_heat_china,
            L224.StubTechSecOut_elec_china,
            L224.StubTechCost_elec_china,
            L224.DeleteSupplysector_CHINAheat,
            L224.StubTechMarket_heat_CHINA)




