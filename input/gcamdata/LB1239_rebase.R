library(gcamdata)
source("./R/constants.R")
source("./R/module-helpers.R")
source("./R/pipeline-helpers.R")
source("./R/utils-data.R")
# source("./rebase/generate_level2.R")
library(dplyr)
library(tidyr)
library(tibble)
library(assertthat)
library(magrittr)
library(readr)

# import data
province_names_mappings <- read_csv("./inst/extdata/gcam-china/province_names_mappings.csv", comment = "#")
L123.out_EJ_province_elec_F <- read_csv("./outputs/L123.out_EJ_province_elec_F.csv", comment = "#")
L1236.grid_elec_supply_CHINA <- read_csv("./outputs/L1236.grid_elec_supply_CHINA.csv", comment = "#")


# ===================================================
# Data Processing

# Create table of electricity generation by load segment | fuel | province
# L123.out_EJ_province_elec_F contains electricity generation by fuel & province
L123.out_EJ_province_elec_F %>%
  # China just has solar PV and no CSP in this dataset, just rename as solar
  mutate(fuel = sub("solar CSP", "solar", fuel)) %>%
  mutate(fuel = sub("solar PV", "solar", fuel)) %>%
  group_by(province, sector, fuel, year) %>%
  summarise(tot_generation = sum(value)) %>%
  ungroup() %>%
  # filter out years which are not present in the electricity load segments calibration data table
  semi_join(L1236.grid_elec_supply_CHINA, by = c("year")) %>%
  left_join_error_no_match(province_names_mappings %>%
                             select(province, grid_region = grid.region),
                           by = "province") %>%
  # map fuel shares by horizontal load segment and grid to the states
  # joining L1236.grid_elec_supply_CHINA is intended to duplicate rows,
  # creating four rows for every state | fuel | year (one row per load segment)
  # left_join_error_no_match throws error when number of rows changes, so left_join is used
  left_join(L1236.grid_elec_supply_CHINA %>%
              select(grid_region, segment, fuel, year, fraction),
            by = c("grid_region", "fuel", "year")) %>%
  mutate(generation = tot_generation * fraction) %>%
  select(province, grid_region, segment, fuel, year, tot_generation, fraction, generation) -> L1239.province_elec_supply

# ===================================================

# Produce outputs

L1239.province_elec_supply %>%
  add_title("Electricity supply by fuel by horizontal load segment in each province.") %>%
  add_units("EJ; unitless (fraction)") %>%
  add_comments("Electricity generation by fuel & province (from L123.out_EJ_province_elec_F) allocated across horizontal load segments.") %>%
  add_comments("This allocation is based on the fraction of fuel in the horizontal load segments by grid region (from L1236.grid_elec_supply_CHINA).") %>%
  add_legacy_name("L1239.state_elec_supply") %>%
  add_precursors("gcam-china/province_names_mappings",
                 "L123.out_EJ_province_elec_F",
                 "L1236.grid_elec_supply_CHINA") ->
  L1239.province_elec_supply_CHINA

return_data(L1239.province_elec_supply_CHINA)





