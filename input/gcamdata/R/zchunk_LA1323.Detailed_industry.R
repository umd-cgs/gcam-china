#' module_gcamchina_LA1323.detailed_industry
#'
#' To calculate national detailed_industry production, energy inputs and Input-output coefficients to provinces
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on /code{command}: either a vector of required inputs,
#' a vector of output names, or (if /code{command} is "MAKE") all
#' the generated outputs: /code{L1323.out_Mt_province_detailed_industry_Yh}, /code{L1323.IO_GJkg_province_detailed_industry_F_Yh}, /code{L1323.in_EJ_province_detailed_industry_F_Y}, /code{L1323.in_EJ_province_indnochp_F}. The corresponding file in the
#' @details The tables for detailed_industry production, i.e., out, and energy inputs, i.e., in, were calculated by applying province shares to national data.
#' @details The province shares were determined by the provinces' relative values of detailed_industry shipments.
#' @details The input-out coefficients were calculated by energy inputs divide outputs.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author YangLiu Sep 2020
module_gcamchina_LA1323.detailed_industry <- function(command, ...) {
 if(command == driver.DECLARE_INPUTS) {
   return(c( FILE = "gcam-china/province_names_mappings",
             FILE = "gcam-china/detailed_industry_output",
             FILE = "gcam-china/IO_detailed_industry",
             "L132.in_EJ_province_indnochp_F",
             "L132.in_EJ_province_indfeed_F"))
 } else if(command == driver.DECLARE_OUTPUTS) {
   return(c("L1323.out_Mt_province_detailed_industry_Yh",
            "L1323.IO_GJkg_province_detailed_industry_F_Yh",
            "L1323.in_EJ_province_detailed_industry_F_Y",
			"L1323.in_EJ_province_indnochp_F",
			"L1323.in_EJ_province_indfeed_F"))
 } else if(command == driver.MAKE) {

   all_data <- list(...)[[1]]

   sector <- fuel <- province <- year <- value <- NULL   # silence package check notes

   # Load required inputs
   detailed_industry_output <- get_data(all_data, "gcam-china/detailed_industry_output", strip_attributes = T)
   province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = T)
   IO_detailed_industry <- get_data(all_data, "gcam-china/IO_detailed_industry", strip_attributes = T)
   L132.in_EJ_province_indnochp_F <- get_data(all_data, "L132.in_EJ_province_indnochp_F", strip_attributes = T)
   L132.in_EJ_province_indfeed_F <- get_data(all_data, "L132.in_EJ_province_indfeed_F", strip_attributes = T)

   # ===================================================
    #produce io,energy,output
    IO_detailed_industry %>%
      rename(value = energy.input) %>%
      select(-coefficient,-production) %>%
	  #filter energy input
      filter(minicam.energy.input %in% c("delivered coal","refined liquids industrial","wholesale gas","H2 enduse",
      "delivered biomass","elect_td_ind","district heat")) ->
    L1323.in_EJ_province_detailed_industry_F_Y

   #split feedstock and energy use
   L1323.in_EJ_province_detailed_industry_F_Y %>%
      filter(supplysector %in% c("chemical feedstock","construction feedstocks") ) ->
      L1323.in_EJ_province_detailed_industry_F_Y_feedstock

   L1323.in_EJ_province_detailed_industry_F_Y %>%
      filter(!(supplysector %in% c("chemical feedstock","construction feedstocks")) ) ->
      L1323.in_EJ_province_detailed_industry_F_Y

    #IO
    IO_detailed_industry %>%
      select(-energy.input,-production) ->
     L1323.IO_GJkg_province_detailed_industry_F_Yh

    #output
    detailed_industry_output %>%
      rename(value = production) ->
    L1323.out_Mt_province_detailed_industry_Yh


    #Calculate the remaining industrial energy use
    L132.in_EJ_province_indnochp_F %>%
      rename(raw = value) %>%
      left_join(L1323.in_EJ_province_detailed_industry_F_Y %>%
                  mutate(minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="delivered coal", "coal"),
                         minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="elect_td_ind", "electricity"),
                         minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="refined liquids industrial", "refined liquids"),
                         minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="wholesale gas", "gas"),
                         minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="district heat", "heat"),
                         minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="delivered biomass", "biomass")) %>%
                  group_by(province, year, minicam.energy.input) %>%
                  summarise(value = sum(value)) %>% ungroup(), by = c("province", "year", fuel = 'minicam.energy.input')) %>%
      replace_na(list(value = 0)) %>%
      mutate(value = replace_na(raw - value,0), raw = NULL) %>%
      # YO 2023
      # remove negative values for HE coal in 2015, check original IO_detailed_industry.csv table and maybe need to update for consistency
      mutate(value = if_else(value < 0, 0, value))->
      L1323.in_EJ_province_indnochp_F

    #Calculate the remaining industrial feedstock
    L132.in_EJ_province_indfeed_F %>%
       rename(raw = value) %>%
       left_join(L1323.in_EJ_province_detailed_industry_F_Y_feedstock %>%
                    mutate(minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="delivered coal", "coal"),
                           minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="refined liquids industrial", "refined liquids"),
                           minicam.energy.input = replace(minicam.energy.input, minicam.energy.input =="wholesale gas", "gas")) %>%
                    group_by(province, year, minicam.energy.input) %>%
                    summarise(value = sum(value)) %>% ungroup(), by = c("province", "year", fuel = 'minicam.energy.input')) %>%
       replace_na(list(value = 0)) %>%
       mutate(value = replace_na(raw - value,0), raw = NULL) ->
       L1323.in_EJ_province_indfeed_F

    # -----------------------------------------------------------------------------
    # 3. Output
    # ===================================================

   L1323.out_Mt_province_detailed_industry_Yh %>%
     add_title("detailed_industry production by province / technology / historical year", overwrite =TRUE) %>%
     add_units("Mt") %>%
       add_comments("detailed_industry production") %>%
     add_legacy_name("L1323.out_Mt_province_detailed_industry_Yh") %>%
     add_precursors("gcam-china/detailed_industry_output", "gcam-china/province_names_mappings") ->
     L1323.out_Mt_province_detailed_industry_Yh

   L1323.IO_GJkg_province_detailed_industry_F_Yh %>%
     add_title("Input-output coefficients of detailed_industry production by province / input / historical year", overwrite =TRUE) %>%
     add_units("GJ/kg and kg/kg") %>%
     add_comments("Calculate IO coefficients for each fuel") %>%
     add_legacy_name("L1323.IO_GJkg_province_detailed_industry_F_Yh") %>%
     add_precursors("gcam-china/IO_detailed_industry", "gcam-china/province_names_mappings") ->
     L1323.IO_GJkg_province_detailed_industry_F_Yh

   L1323.in_EJ_province_detailed_industry_F_Y %>%
     add_title("Energy inputs to detailed_industry production by province / fuel / historical year", overwrite =TRUE) %>%
     add_units("EJ/yr") %>%
     add_comments("these province shares were calculated to be proportional to the their values of detailed_industry production") %>%
     add_legacy_name("L1323.in_EJ_province_detailed_industry_F_Y") %>%
     add_precursors( "gcam-china/province_names_mappings", "gcam-china/detailed_industry_output") ->
     L1323.in_EJ_province_detailed_industry_F_Y


   L1323.in_EJ_province_indnochp_F %>%
     add_title("Energy inputs in industry sector by province / fuel / historical year") %>%
     add_units("EJ/yr") %>%
     add_comments("these province shares were calculated to be proportional to the their values of detailed_industry production") %>%
     add_legacy_name("L1323.in_EJ_province_indnochp_F") %>%
     add_precursors( "gcam-china/province_names_mappings", "L132.in_EJ_province_indnochp_F","gcam-china/IO_detailed_industry","gcam-china/detailed_industry_output") ->
     L1323.in_EJ_province_indnochp_F

   L1323.in_EJ_province_indfeed_F %>%
      add_title("Feedstock in industry sector by province / fuel / historical year") %>%
      add_units("EJ/yr") %>%
      add_comments("these province shares were calculated to be proportional to the their values of detailed_industry production") %>%
      add_legacy_name("L1323.in_EJ_province_indfeed_F") %>%
      add_precursors( "gcam-china/province_names_mappings", "L132.in_EJ_province_indfeed_F","gcam-china/IO_detailed_industry","gcam-china/detailed_industry_output") ->
      L1323.in_EJ_province_indfeed_F

   return_data(L1323.out_Mt_province_detailed_industry_Yh, L1323.IO_GJkg_province_detailed_industry_F_Yh, L1323.in_EJ_province_detailed_industry_F_Y,L1323.in_EJ_province_indnochp_F,L1323.in_EJ_province_indfeed_F)
 } else {
   stop("Unknown command")
 }
}
