#' module_gcam.china_LA161.Cstorage
#'
#' Calculates onshore CO2 storage by grid region.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L161.Cstorage_province}. The corresponding file in the
#' original data system was \code{LA161.Cstorage.R} (gcam-china level1).
#' @details Calculates onshore CO2 storage by grid region.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author Liu August 2018

module_gcam.china_LA161.Cstorage <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "gcam-china/CCS_provincial_data",
             FILE = "gcam-china/CCS_provincial_data_add",
             FILE = "gcam-china/unallocated_CStorage_province",
             FILE = "gcam-china/CCS_potential_TU_Scailing"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L161.Cstorage_province"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    province <- province.name <- grid_region <- Cost_2005USDtCO2 <- CO2_Mt <- MtC <- MtCO2 <- Cumul_MtC <- quantile <-
      grade <- Cost_1990USDtC <- add_storage <- NULL

    all_data <- list(...)[[1]]

    # -----------------------------------------------------------------------------
    # 1.Load required inputsMtCO2
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings")
    CCS_China_province      <- get_data(all_data, "gcam-china/CCS_provincial_data")
    CCS_China_province_add  <- get_data(all_data, "gcam-china/CCS_provincial_data_add")
    Unallocated_CStorage    <- get_data(all_data, "gcam-china/unallocated_CStorage_province")
    CCS_potential_TU_Scailing <- get_data(all_data, "gcam-china/CCS_potential_TU_Scailing")
    # -----------------------------------------------------------------------------
    # 2.perform computations
    CCS_China_province %>%
      # Add grid.region
      left_join_error_no_match(province_names_mappings, by = "province.name") %>%
      # Convert transport and storage costs from 2005 USD per ton of CO2 to 1990 USD per ton of C
      # Convert storage unit from Mt CO2 to Mt C
      mutate(Cost_1990USDtC = Cost_2005USDtCO2 * emissions.CONV_C_CO2 / gdp_deflator(2005, 1990),
             MtC = CO2_Mt * (1 / emissions.CONV_C_CO2)) %>%
      arrange(province.name, Cost_2005USDtCO2) %>%

      # Removing data for provinces without enough onshore storage
      filter(!(province.name %in% c("Fujian", "Guangdong", "Guangxi", "Qinghai"))) %>%
      group_by(province.name) %>%

      # Calculate cumulative sum, then filter to the quantiles
      mutate(Cumul_MtC = cumsum(MtC)) %>%

      # use "type=1" to indicate that we want to return only exact matches to the data in the table.
      # only take the (0, 0.2, 0.4, 0.6, 0.8, 1) points in the quartiles

      filter(Cumul_MtC %in% quantile(Cumul_MtC, c(0, 0.2, 0.4, 0.6, 0.8, 1), type = 1)) %>%
      # this will create grade equal to grade 1 for 0, grade 2 for 0.2, etc.
      # from the cumulative totals, return to the marginal quantities associated with each grade
      mutate(grade = paste("grade", 1:6, sep = " "),
             MtC = Cumul_MtC - lag(Cumul_MtC, 1, 0)) %>%
      ungroup %>%

      # Return from the list to a data frame with all necessary data
      select(province.name, grade, Cost_1990USDtC, MtC) %>%

      # Add cost grades to provinces without data
      # i.e. Fujian, Guangdong, Guangxi, Qinghai, Hainan, and Tibet
      bind_rows(CCS_China_province_add) %>%

      # Setting a minimum cost of 0 on CO2 storage and transport projects
      mutate(Cost_1990USDtC = pmax(Cost_1990USDtC, 0)) %>%

      # Manually changing cost curves to avoid grades 2 or 3 being 0. Value determined by linear interpolation. If the data changes in the future, will need to change this
      mutate(Cost_1990USDtC = replace(Cost_1990USDtC, province.name == "Jilin" & grade == "grade 2", 4.625),
             Cost_1990USDtC = replace(Cost_1990USDtC, province.name == "Xinjiang" & grade == "grade 2", 4.3),
             Cost_1990USDtC = replace(Cost_1990USDtC, province.name == "Yunnan" & grade == "grade 2", 5.74),
             Cost_1990USDtC = replace(Cost_1990USDtC, province.name == "Yunnan" & grade == "grade 3", 11.48)) ->
      L161.Cstorage_province

    # Extending the plateau (bin 2) of the cost curve using additional resource data
    L161.Cstorage_province %>%
      filter(grade == "grade 2") %>%
      left_join_error_no_match(Unallocated_CStorage, by = "province.name") %>%

      # Convert to MtC for additional resource
      mutate(add_storage = MtCO2 * (1 / emissions.CONV_C_CO2),

             # Gansu is a special case, we are not adding storage, see the China-CCS paper.
             add_storage = replace(add_storage, province.name == "Gansu", 0),
             MtC = MtC + add_storage) %>%
      select(province.name, grade, Cost_1990USDtC, MtC) ->
      L161.Cstorage_province_g2

    # section devided
    L161.Cstorage_province %>%
      filter(grade != "grade 2") %>%
      bind_rows(L161.Cstorage_province_g2) %>%
      arrange(province.name, grade) %>%
      map_province_name(province_names_mappings, "province", FALSE) %>%
      select(province, province.name, grade, Cost_1990USDtC, MtC) ->
      L161.Cstorage_province

    # Downscailing
    L161.Cstorage_province <- CCS_potential_TU_Scailing %>%
    rename(province.name = Province) %>%
    left_join(L161.Cstorage_province, by ="province.name") %>%
    group_by(province.name) %>%
    mutate(GCAM=sum(MtC*(44/12))) %>%
    ungroup() %>%
    mutate(Scailing_ratio=(Direct_filter_prohibited_indicators/GCAM),
           MtC=MtC*Scailing_ratio, MtC = if_else(is.na(MtC),0,MtC))%>%
      select(-c("province.name","GCAM","Unfiltered", "Filter_prohibited_indicators _by_county","Direct_filter_prohibited_indicators",
                "Yiming_Wie_JCP2022", "Note", "Scailing_ratio"))%>%
      drop_na()

    # ===================================================
    # Produce outputs
    L161.Cstorage_province %>%
      add_title("CO2 storage curves by province and grade") %>%
      add_units("MtC; 1990 USD per tC") %>%
      add_comments("Cumulative MtC calculated by province, then filtered to quartiles") %>%
      add_legacy_name("L161.Cstorage_province") %>%
      add_precursors("gcam-china/province_names_mappings",
                    "gcam-china/CCS_provincial_data",
                    "gcam-china/unallocated_CStorage_province",
                    "gcam-china/CCS_provincial_data_add",
                    "gcam-china/CCS_potential_TU_Scailing") ->
      L161.Cstorage_province

    return_data(L161.Cstorage_province)
  } else {
    stop("Unknown command")
  }
}
