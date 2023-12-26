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
L101.inNBS_Mtce_province_S_F <- read_csv("./outputs/L101.inNBS_Mtce_province_S_F.csv", comment = "#")
L101.NBS_use_all_Mtce <- read_csv("./outputs/L101.NBS_use_all_Mtce.csv", comment = "#")
L122.in_EJ_province_refining_F <- read_csv("./outputs/L122.in_EJ_province_refining_F.csv", comment = "#")
L123.in_EJ_R_indchp_F_Yh <- read_csv("./outputs/L123.in_EJ_R_indchp_F_Yh.csv", comment = "#")
L123.out_EJ_R_indchp_F_Yh <- read_csv("./outputs/L123.out_EJ_R_indchp_F_Yh.csv", comment = "#")
L1322.in_EJ_R_indenergy_F_Yh <- read_csv("./outputs/L1322.in_EJ_R_indenergy_F_Yh.csv", comment = "#")
L1322.in_EJ_R_indfeed_F_Yh <- read_csv("./outputs/L1322.in_EJ_R_indfeed_F_Yh.csv", comment = "#")

L2234.StubTechProd_elecS_CHINA <- read_csv("./outputs/L2234.StubTechProd_elecS_CHINA.csv", comment = "#")


# ===================================================

# PART 1. Compute industrial energy use, removing energy used in refining

# Aggregate all refining fuel consumption for electricity and gas by province
# TODO: skipping province industrial refining energy use for now due to units issues
if(1 == 2) {
  #Electricity and gas inputs to refineries are now deducted from industry.
  #Subset industrial energy use from the China table
  L122.in_EJ_province_refining_F %>%
    filter(fuel %in% c("electricity", "gas")) %>%
    group_by(province, fuel, year) %>%
    summarise(refinery_comsumption = sum(value)) %>%
    ungroup ->
    L132.in_EJ_province_refining_elecgas

  # Adjust industrial fuel consumption by removing refinery consumption computed above
  L101.inNBS_Mtce_province_S_F %>%
    filter(sector == "industry", fuel %in% c("electricity", "gas")) %>%
    # We need to transfer the Mtce to EJ. We haven't done that yet.
    left_join_error_no_match(L132.in_EJ_province_refining_elecgas, by = c("province", "fuel", "year")) %>%
    #Replace any negative values with zeroes
    mutate(value = value - refinery_comsumption,
           value = replace(value, value < 0, 0)) %>%
    select(-refinery_comsumption) ->
    L132.in_EJ_province_ind_elecgas_adj
}

# Put together a new table for industrial energy consumption. This will be used to calculate province-wise percentages of industrial energy use
# Need to add biomass use by province since the CESY does not include it
#just using coal shares for now
L101.inNBS_Mtce_province_S_F %>%
  filter(sector == "industry", fuel == "coal") %>%
  mutate(fuel = 'biomass') ->
  L132.in_Mtce_province_indenergy_F_unscaled.bio

# Only use the fuels that are considered in GCAM's "industrial energy use" sector
L101.inNBS_Mtce_province_S_F %>%
  filter(sector == "industry",
         fuel %in% unique(L1322.in_EJ_R_indenergy_F_Yh$fuel)) %>%
  bind_rows(L132.in_Mtce_province_indenergy_F_unscaled.bio) %>%
  complete(nesting(sector,fuel), province, year = HISTORICAL_YEARS) %>%
  group_by(province, year) %>%
  # First zero out NAs in years where some values are NA but not all
  mutate(value = replace(value, is.na(value) & sum(value, na.rm = T) != 0, 0)) %>%
  ungroup %>%
  # use approx_fun rule = 2 to fill out data in years where the entire province is NA
  group_by(province, sector, fuel) %>%
  mutate(value = approx_fun(year, value, rule = 2), sum = NULL) %>%
  ungroup() ->
  L132.in_Mtce_province_indenergy_F_unscaled


# Compute fuel consumption by province and sector as proportion of China total
L132.in_Mtce_province_indenergy_F_unscaled %>%
  group_by(sector, fuel, year) %>%
  mutate(multiplier = value / sum(value), value = NULL) %>%
  replace_na(list(multiplier = 0)) %>%
  ungroup ->
  L132.in_pct_province_ind_F


# PART 2. Apportion China consumption and output to province level for non-cogeneration and cogeneration
# Apportion national-level industrial energy consumption to provinces - NON-COGEN
L132.in_pct_province_ind_F %>%
  mutate(sector = "industry_energy") %>%
  # Prepare for smooth join
  left_join(filter(L1322.in_EJ_R_indenergy_F_Yh, GCAM_region_ID == gcamchina.REGION_ID),
            by = c("fuel", "year", "sector")) %>%
  mutate(value = value *  multiplier) ->
  #select(-multiplier, -GCAM_region_ID) ->
  L132.in_EJ_province_indnochp_F

# Apportion national-level industrial energy consumption to provinces - COGEN
# Industrial sector cogeneration input energy by province and fuel

# YO Dec 2023
# for COGEN need to adjust based on CHP heat output from gas steam/CT
# same as L224 chunk
no_gas_tech_2010 <- L2234.StubTechProd_elecS_CHINA %>%
  filter(year == 2010 & subsector == "gas") %>%
  filter(grepl("steam/CT", stub.technology)) %>%
  group_by(region, year) %>%
  summarise(no_gas = sum(calOutputValue)) %>%
  ungroup() %>%
  filter(no_gas == 0)

L132.in_pct_province_ind_F_cogen <- L132.in_pct_province_ind_F %>%
  mutate(multiplier_new = if_else(province %in% no_gas_tech_2010$region & year == 2010 & fuel == "gas", 0, multiplier)) %>%
  group_by(sector, fuel, year) %>%
  mutate(multiplier_adj = multiplier_new / sum(multiplier_new)) %>%
  ungroup() %>%
  select(-multiplier, -multiplier_new) %>%
  select(sector, fuel, province, year, multiplier = multiplier_adj)

L132.in_pct_province_ind_F_cogen %>%
  filter(fuel %in% unique(L123.in_EJ_R_indchp_F_Yh$fuel)) %>%
  # We only want fuels that are inputs to cogen systems, i.e. not electricity
  mutate(sector = "chp_elec") %>%
  left_join_error_no_match(filter(L123.in_EJ_R_indchp_F_Yh, GCAM_region_ID == gcamchina.REGION_ID),
                           by = c("fuel", "year", "sector")) %>%
  mutate(value = value * multiplier) %>%
  select(-multiplier, -GCAM_region_ID) ->
  L132.in_EJ_province_indchp_F

# Apportion nation-level industrial cogen output to provinces
L132.in_pct_province_ind_F %>%
  filter(fuel %in% gcam.IND_ENERGY_USE) %>%
  mutate(sector = "chp_elec") %>%
  # ^^ prepare for smooth join
  left_join_error_no_match(filter(L123.out_EJ_R_indchp_F_Yh, GCAM_region_ID == gcamchina.REGION_ID),
                           by = c("fuel", "year", "sector")) %>%
  mutate(value = value * multiplier) %>%
  select(-multiplier, -GCAM_region_ID) ->
  L132.out_EJ_province_indchp_F


# PART 3: Industrial feedstocks
# Feedstocks are the fuel inputs to the CESY Industry -- NonEnergy plus asphalt input into construction (for roads)
# Split province-level data into feedstocks only, according to names of fuel inputs

L101.NBS_use_all_Mtce %>%
  filter(EBProcess == "Construction",
         EBMaterial == "Bitumen Asphalt") %>%
  mutate(sector = 'industry_feedstocks', fuel = "refined liquids") ->
  L132.in_Mtce_province_indfeed_F_unscaled.road

L101.inNBS_Mtce_province_S_F %>%
  filter(sector == "industry_feedstocks",
         fuel %in% unique(L1322.in_EJ_R_indfeed_F_Yh$fuel)) %>%
  bind_rows(L132.in_Mtce_province_indfeed_F_unscaled.road[, names( L101.inNBS_Mtce_province_S_F)]) %>%
  # First zero out NAs in years where some values are NA but not all
  group_by(province, year) %>%
  mutate(value = replace(value, is.na(value) & sum(value, na.rm = T) != 0, 0)) %>%
  ungroup %>%
  group_by(province, sector, fuel, year) %>%
  summarise(value = sum(value)) %>%
  ungroup ->
  L132.in_Mtce_province_indfeed_F_unscaled

L132.in_Mtce_province_indfeed_F_unscaled %>%
  group_by(province, sector, fuel) %>%
  # use approx_fun rule = 2 to fill out data in years where the entire province is NA
  mutate(value = approx_fun(year, value, rule = 2)) %>%
  ungroup %>%
  # When there is one data point, approx_fun replaced it with NA, but what we needed is to replace all NAs with that data.
  left_join(L132.in_Mtce_province_indfeed_F_unscaled %>% rename(org = value),
            by = c("province", "fuel", "sector", "year")) %>%
  group_by(sector, fuel, province) %>%
  # Only under the occasion when there is one data point in the group,
  # approx_fun replaces the one value with NA.
  # The original gcam_interp function replaces all missing values in that group with the one value when rule = 2.
  mutate(value = replace(value, is.na(value) & sum(org[is.na(value)], na.rm = T) != 0,
                         sum(org[is.na(value)], na.rm = T))) %>%
  select(-org) %>%
  ungroup %>%
  group_by(sector, fuel, year) %>%
  mutate(multiplier = value / sum(value, na.rm = T), value = NULL) %>%
  replace_na(list(multiplier = 0)) %>%
  ungroup ->
  L132.in_pct_province_indfeed_F

# Apportion feedstocks among provinces
L132.in_pct_province_indfeed_F %>%
  # Prepare for smooth join
  left_join_error_no_match(filter(L1322.in_EJ_R_indfeed_F_Yh, GCAM_region_ID == gcamchina.REGION_ID),
                           by = c("sector", "fuel", "year")) %>%
  mutate(value = value * multiplier) %>%
  # Get province portions
  select(-multiplier, -GCAM_region_ID) %>%
  arrange(province) ->
  L132.in_EJ_province_indfeed_F


## OUTPUTS
L132.in_EJ_province_indnochp_F %>%
  add_title("Industrial sector non-cogen input energy by province and fuel") %>%
  add_units("EJ") %>%
  add_comments("Computed by apportioning China-level consumption among provinces") %>%
  add_legacy_name("L132.in_EJ_province_indnochp_F") %>%
  add_precursors("L1322.in_EJ_R_indenergy_F_Yh",
                 "L101.inNBS_Mtce_province_S_F",
                 "L122.in_EJ_province_refining_F") ->
  L132.in_EJ_province_indnochp_F

L132.in_EJ_province_indchp_F %>%
  add_title("Industrial sector cogeneration input energy by province and fuel") %>%
  add_units("EJ") %>%
  add_comments("Computed by apportioning China-level consumption among provinces") %>%
  add_legacy_name("L132.in_EJ_province_indchp_F") %>%
  add_precursors("L123.out_EJ_R_indchp_F_Yh",
                 "L101.inNBS_Mtce_province_S_F",
                 "L122.in_EJ_province_refining_F") ->
  L132.in_EJ_province_indchp_F

L132.out_EJ_province_indchp_F %>%
  add_title("Industrial sector electricity cogeneration by province") %>%
  add_units("EJ") %>%
  add_comments("Computed by apportioning China-level CHP generation among provinces") %>%
  add_legacy_name("L132.out_EJ_province_indchp_F") %>%
  add_precursors("L123.in_EJ_R_indchp_F_Yh",
                 "L101.inNBS_Mtce_province_S_F",
                 "L122.in_EJ_province_refining_F") ->
  L132.out_EJ_province_indchp_F

L132.in_EJ_province_indfeed_F %>%
  add_title("Industrial feedstocks by province and fuel") %>%
  add_units("EJ") %>%
  add_comments("Computed by apportioning China-level feedstocks among provinces") %>%
  add_legacy_name("L132.in_EJ_province_indfeed_F") %>%
  add_precursors("L101.inNBS_Mtce_province_S_F",
                 "L1322.in_EJ_R_indfeed_F_Yh",
                 "L101.NBS_use_all_Mtce") ->
  L132.in_EJ_province_indfeed_F

return_data(L132.in_EJ_province_indnochp_F, L132.in_EJ_province_indchp_F, L132.out_EJ_province_indchp_F, L132.in_EJ_province_indfeed_F)





