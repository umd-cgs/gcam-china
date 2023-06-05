#' module_gcamchina_L210.Resources_china
#'
#' GCAM-CHINA resource market information, prices, TechChange parameters, and supply curves.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L210.RenewRsrc_CHINA}, \code{L210.RenewRsrcPrice_CHINA}, \code{L210.UnlimitRsrc_CHINA},\code{L210.UnlimitRsrc_limestone_CHINA},
#' \code{L210.UnlimitRsrcPrice_CHINA}, \code{L210.UnlimitRsrcPrice_limestone_CHINA},\code{L210.SmthRenewRsrcCurves_wind_CHINA}, \code{L210.SmthRenewRsrcTechChange_CHINA},
#' \code{L210.SmthRenewRsrcTechChange_offshore_wind_CHINA}, \code{L210.SmthRenewRsrcCurves_offshore_wind_CHINA}, \code{L210.GrdRenewRsrcCurves_geo_CHINA}, \code{L210.GrdRenewRsrcMax_geo_CHINA}, \code{L210.SmthRenewRsrcCurvesGdpElast_roofPV_CHINA}, \code{L210.ResTechShrwt_CHINA}
#' The corresponding file in the original data system was \code{L210.resources_CHINA.R} (gcam-china level2).
#' @details GCAM-China resource market information, prices, TechChange parameters, and supply curves.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BY June 2019

module_gcamchina_L210.Resources_china <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "gcam-china/wind_potential_province",
             "L1321.out_Mt_province_cement_Yh",
             "L1231.out_EJ_province_elec_F_tech",
             "L210.RenewRsrc",
             "L210.RenewRsrcPrice",
             "L210.UnlimitRsrc",
             "L210.UnlimitRsrcPrice",
             "L210.SmthRenewRsrcTechChange",
             "L210.SmthRenewRsrcCurves_wind",
             "L210.SmthRenewRsrcCurvesGdpElast_roofPV",
             "L210.GrdRenewRsrcCurves_geo",
             "L210.GrdRenewRsrcMax_geo",
             "L210.SmthRenewRsrcCurves_offshore_wind",
             "L210.SmthRenewRsrcTechChange_offshore_wind"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L210.RenewRsrc_CHINA",
             "L210.RenewRsrcPrice_CHINA",
             "L210.UnlimitRsrc_CHINA",
             "L210.UnlimitRsrc_limestone_CHINA",
             "L210.UnlimitRsrcPrice_CHINA",
             "L210.UnlimitRsrcPrice_limestone_CHINA",
             "L210.SmthRenewRsrcTechChange_CHINA",
             "L210.SmthRenewRsrcTechChange_offshore_wind_CHINA",
             "L210.SmthRenewRsrcCurves_wind_CHINA",
             "L210.SmthRenewRsrcCurves_offshore_wind_CHINA",
             "L210.ResTechShrwt_CHINA",
             "L210.GrdRenewRsrcCurves_geo_CHINA",
             "L210.GrdRenewRsrcMax_geo_CHINA",
             "L210.SmthRenewRsrcCurvesGdpElast_roofPV_CHINA"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    curve.exponent <- maxResource <- maxSubResource <- mid.price <-
      region <- renewresource <- smooth.renewable.subresource <-
      unlimited.resource <- year.fillout <- province <- fuel <- year <-
      value <- . <- sub.renewable.resource <- subresource <- NULL

    # Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = T)
    wind_potential_province <- get_data( all_data, "gcam-china/wind_potential_province", strip_attributes = T)
    L1321.out_Mt_province_cement_Yh <- get_data(all_data, "L1321.out_Mt_province_cement_Yh", strip_attributes = T)
    L1231.out_EJ_province_elec_F_tech <- get_data(all_data, "L1231.out_EJ_province_elec_F_tech", strip_attributes = T)
    L210.RenewRsrc <- get_data(all_data, "L210.RenewRsrc", strip_attributes = T)
    L210.RenewRsrcPrice <- get_data(all_data, "L210.RenewRsrcPrice", strip_attributes = T)
    L210.UnlimitRsrc <- get_data(all_data, "L210.UnlimitRsrc", strip_attributes = T)
    L210.UnlimitRsrcPrice <- get_data(all_data, "L210.UnlimitRsrcPrice", strip_attributes = T)
    L210.SmthRenewRsrcTechChange <- get_data(all_data, "L210.SmthRenewRsrcTechChange", strip_attributes = T)
    L210.SmthRenewRsrcCurves_wind <- get_data(all_data, "L210.SmthRenewRsrcCurves_wind", strip_attributes = T)
    L210.SmthRenewRsrcCurvesGdpElast_roofPV <- get_data(all_data, "L210.SmthRenewRsrcCurvesGdpElast_roofPV", strip_attributes = T)
    L210.GrdRenewRsrcCurves_geo <- get_data(all_data, "L210.GrdRenewRsrcCurves_geo", strip_attributes = T)
    L210.GrdRenewRsrcMax_geo <- get_data(all_data, "L210.GrdRenewRsrcMax_geo", strip_attributes = T)
    L210.SmthRenewRsrcCurves_offshore_wind <- get_data(all_data, "L210.SmthRenewRsrcCurves_offshore_wind", strip_attributes = T)
    L210.SmthRenewRsrcTechChange_offshore_wind <- get_data(all_data, "L210.SmthRenewRsrcTechChange_offshore_wind", strip_attributes = T)

    # ===================================================
    cement_provinces <- unique( L1321.out_Mt_province_cement_Yh$province )

    L1231.out_EJ_province_elec_F_tech %>%
      filter(fuel == "geothermal", (year == 2010 & value == 0)) %>%
      select(province) %>%
      rename(region = province) %>%
      mutate(renewresource = "geothermal") ->
      no_geo_provinces_resource

    # L210.RenewRsrc_CHINA: renewable resource info in the provinces
    L210.RenewRsrc_CHINA <- L210.RenewRsrc %>%
      filter(region == "China",
             renewresource %in% gcamchina.PROVINCE_RENEWABLE_RESOURCES) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["RenewRsrc"]], gcamchina.PROVINCES_ALL) %>%
      # Remove geothermal from provinces that don't have it
      anti_join(no_geo_provinces_resource, by = c("region", "renewresource")) %>%
      mutate(market = if_else(renewresource %in% c("distributed_solar", "geothermal"), "China", region))

    # L210.RenewRsrcPrice_CHINA: unlimited resource prices in the provinces
    # NOTE: Don't know if this is actually needed
    L210.RenewRsrcPrice_CHINA <- L210.RenewRsrcPrice %>%
      filter(region == "China",
             renewresource %in% gcamchina.PROVINCE_RENEWABLE_RESOURCES) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["RenewRsrcPrice"]], gcamchina.PROVINCES_ALL) %>%
      # Remove geothermal from provinces that don't have it
      anti_join(no_geo_provinces_resource, by = c("region", "renewresource"))

    # L210.UnlimitRsrc_CHINA: unlimited resource info in the provinces
    # TODO: If needed, add in capacity factor (from old data system, seems to be 0.3 for solar, 0 for limestone)
    L210.UnlimitRsrc_CHINA <- L210.UnlimitRsrc %>%
      filter(region == "China",
             unlimited.resource %in% gcamchina.PROVINCE_UNLIMITED_RESOURCES) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["UnlimitRsrc"]], gcamchina.PROVINCES_ALL)

    L210.UnlimitRsrc_limestone_CHINA <- L210.UnlimitRsrc_CHINA %>%
      filter(unlimited.resource == "limestone",
             region %in% cement_provinces)

    L210.UnlimitRsrc_CHINA <- L210.UnlimitRsrc_CHINA %>%
      filter(unlimited.resource != "limestone")

    # L210.UnlimitRsrcPrice_CHINA: unlimited resource prices in the provinces
    L210.UnlimitRsrcPrice_CHINA <- L210.UnlimitRsrcPrice %>%
      filter(region == "China",
             unlimited.resource %in% gcamchina.PROVINCE_UNLIMITED_RESOURCES) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["UnlimitRsrcPrice"]], gcamchina.PROVINCES_ALL)

    L210.UnlimitRsrcPrice_limestone_CHINA <- L210.UnlimitRsrcPrice_CHINA %>%
      filter(unlimited.resource == "limestone",
             region %in% cement_provinces)

    L210.UnlimitRsrcPrice_CHINA <- L210.UnlimitRsrcPrice_CHINA %>%
      filter(unlimited.resource != "limestone")

    # L210.SmthRenewRsrcTechChange_CHINA: smooth renewable resource tech change
    L210.SmthRenewRsrcTechChange_CHINA <- L210.SmthRenewRsrcTechChange %>%
      filter(region == gcamchina.REGION,
             renewresource %in% gcamchina.PROVINCE_RENEWABLE_RESOURCES) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["SmthRenewRsrcTechChange"]], gcamchina.PROVINCES_ALL) %>%
      # If geothermal is included in this table, remove provinces that don't exist
      anti_join(no_geo_provinces_resource, by = c("region", "renewresource"))

    # L210.GrdRenewRsrcCurves_geo_CHINA: geothermal resource curves in the provinces
    L210.GrdRenewRsrcCurves_geo_CHINA <- L210.GrdRenewRsrcCurves_geo %>%
      filter(region == gcamchina.REGION) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["RenewRsrcCurves"]], gcamchina.PROVINCES_ALL) %>%
      # If geothermal is included in this table, remove provinces that don't exist
      anti_join(no_geo_provinces_resource, by = c("region", "renewresource"))

    # Maximum resources: currently assuming this is just set to 1, and the resource info is stored in the grades
    # L210.GrdRenewRsrcMax_geo_CHINA: max sub resource for geothermal (placeholder)
    L210.GrdRenewRsrcMax_geo_CHINA <- L210.GrdRenewRsrcMax_geo %>%
      filter(region == gcamchina.REGION) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["GrdRenewRsrcMax"]], gcamchina.PROVINCES_ALL) %>%
      # If geothermal is included in this table, remove provinces that don't exist
      anti_join(no_geo_provinces_resource, by = c("region", "renewresource"))

    # L210.SmthRenewRsrcCurves_wind_CHINA: wind resource curves in the provinces
    L210.SmthRenewRsrcCurves_wind_CHINA <- L210.SmthRenewRsrcCurves_wind %>%
      filter(region == "China") %>%
      repeat_add_columns(tibble(province = gcamchina.PROVINCES_NOHKMC)) %>%
      left_join_error_no_match(province_names_mappings, by = "province") %>%
      select(-maxSubResource, -mid.price, -curve.exponent) %>%
      # Add in new maxSubResource, mid.price, and curve.exponent from wind_potential_province
      left_join_error_no_match(wind_potential_province, by = c("province.name")) %>%
      # Convert wind_potential_province units from 2007$/kWh to 1975$/GJ
      mutate(mid.price = mid.price * gdp_deflator(1975, 2007) / CONV_KWH_GJ) %>%
      select(region = province, renewresource, smooth.renewable.subresource, year.fillout,
             maxSubResource = maxResource, mid.price, curve.exponent)

    # L210.SmthRenewRsrcTechChange_offshore_wind_CHINA: technological change for offshore wind
    L210.SmthRenewRsrcTechChange_offshore_wind_CHINA <- L210.SmthRenewRsrcTechChange_offshore_wind %>%
      filter(region == gcamchina.REGION) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["SmthRenewRsrcTechChange"]], gcamchina.PROVINCES_NOHKMC)

    # L210.SmthRenewRsrcCurves_offshore_wind_CHINA: supply curves of offshore wind resources in the provinces
    L210.SmthRenewRsrcCurves_offshore_wind_CHINA <- L210.SmthRenewRsrcCurves_offshore_wind %>%
      filter(region == gcamchina.REGION) %>%
      select(-region) %>%
      repeat_add_columns(tibble(province = gcamchina.PROVINCES_NOHKMC)) %>%
      select(region = province, renewresource, smooth.renewable.subresource, maxSubResource, mid.price, curve.exponent, year.fillout)

    # L210.SmthRenewRsrcCurvesGdpElast_roofPV_CHINA: rooftop PV resource curves in the provinces
    L210.SmthRenewRsrcCurvesGdpElast_roofPV_CHINA <- L210.SmthRenewRsrcCurvesGdpElast_roofPV %>%
      filter(region == gcamchina.REGION) %>%
      write_to_all_provinces(LEVEL2_DATA_NAMES[["SmthRenewRsrcCurvesGdpElast"]], gcamchina.PROVINCES_ALL)

    # L210.ResTechShrwt_China: To provide a shell for the technology object in the resources
    L210.SmthRenewRsrcCurves_wind_CHINA %>%
      select(region, resource = renewresource, subresource = smooth.renewable.subresource) %>%
      bind_rows(select(L210.GrdRenewRsrcMax_geo_CHINA, region, resource = renewresource, subresource = sub.renewable.resource)) %>%
      bind_rows(select(L210.SmthRenewRsrcCurvesGdpElast_roofPV_CHINA, region, resource = renewresource, subresource = smooth.renewable.subresource)) %>%
      bind_rows(select(L210.SmthRenewRsrcCurves_offshore_wind_CHINA, region, resource = renewresource, subresource = smooth.renewable.subresource)) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subresource,
             share.weight = 1.0) %>%
      select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
      L210.ResTechShrwt_CHINA
    # ===================================================

    # Produce outputs
    L210.RenewRsrc_CHINA %>%
      add_title("Renewable resource info in the provinces") %>%
      add_units("NA") %>%
      add_comments("L210.RenewRsrc filtered and written to all provinces") %>%
      add_legacy_name("L210.RenewRsrc_CHINA") %>%
      add_precursors("L210.RenewRsrc", "L1231.out_EJ_province_elec_F_tech") ->
      L210.RenewRsrc_CHINA

    L210.RenewRsrcPrice_CHINA %>%
      add_title("Renewable resource prices in the provinces") %>%
      add_units("1975$/GJ") %>%
      add_comments("L210.RenewRsrcPrice filtered and written to all provinces") %>%
      add_legacy_name("L210.RenewRsrcPrice_CHINA") %>%
      add_precursors("L210.RenewRsrcPrice") ->
      L210.RenewRsrcPrice_CHINA

    L210.UnlimitRsrc_CHINA %>%
      add_title("Unlimited resource info in the provinces") %>%
      add_units("NA") %>%
      add_comments("L210.UnlimitRsrc filtered and written to all provinces") %>%
      add_legacy_name("L210.UnlimitRsrc_CHINA") %>%
      add_precursors("L210.UnlimitRsrc") ->
      L210.UnlimitRsrc_CHINA

    L210.UnlimitRsrc_limestone_CHINA %>%
      add_title("Limestone info in the provinces") %>%
      add_units("NA") %>%
      add_comments("L210.UnlimitRsrc filtered and written to all provinces") %>%
      add_legacy_name("L210.UnlimitRsrc_limestone_CHINA") %>%
      add_precursors("L210.UnlimitRsrc", "L1321.out_Mt_province_cement_Yh") ->
      L210.UnlimitRsrc_limestone_CHINA

    L210.UnlimitRsrcPrice_CHINA %>%
      add_title("Unlimited resource prices in the provinces") %>%
      add_units("1975$/GJ") %>%
      add_comments("L210.UnlimitRsrcPrice filtered and written to all provinces") %>%
      add_legacy_name("L210.UnlimitRsrcPrice_CHINA") %>%
      add_precursors("L210.UnlimitRsrcPrice") ->
      L210.UnlimitRsrcPrice_CHINA

    L210.UnlimitRsrcPrice_limestone_CHINA %>%
      add_title("Limestone prices in the provinces") %>%
      add_units("1975$/kg") %>%
      add_comments("L210.UnlimitRsrcPrice filtered and written to all provinces") %>%
      add_legacy_name("L210.UnlimitRsrcPrice_limestone_CHINA") %>%
      add_precursors("L210.UnlimitRsrcPrice", "L1321.out_Mt_province_cement_Yh") ->
      L210.UnlimitRsrcPrice_limestone_CHINA

    L210.SmthRenewRsrcTechChange_CHINA %>%
      add_title("Smooth renewable resource tech change: China") %>%
      add_units("Unitless") %>%
      add_comments("L210.SmthRenewRsrcTechChange filtered and written to all provinces") %>%
      add_legacy_name("NA") %>%
      add_precursors("L210.SmthRenewRsrcTechChange") ->
      L210.SmthRenewRsrcTechChange_CHINA

    L210.SmthRenewRsrcTechChange_offshore_wind_CHINA %>%
      add_title("Technological change parameter for offshore wind resource") %>%
      add_units("Unitless") %>%
      add_comments("Data from L210.SmthRenewRsrcTechChange_offshore_wind_CHINA added to all provinces") %>%
      add_legacy_name("NA") %>%
      add_precursors("L210.SmthRenewRsrcTechChange_offshore_wind") ->
      L210.SmthRenewRsrcTechChange_offshore_wind_CHINA

    L210.SmthRenewRsrcCurves_wind_CHINA %>%
      add_title("Wind resource curves in the provinces") %>%
      add_units("maxSubResource: EJ; mid.price: 1975$/GJ") %>%
      add_comments("L210.SmthRenewRsrcCurves_wind filtered and written to all provinces") %>%
      add_legacy_name("L210.SmthRenewRsrcCurves_wind_provinces") %>%
      add_precursors("L210.SmthRenewRsrcCurves_wind", "gcam-china/wind_potential_province", "gcam-china/province_names_mappings") ->
      L210.SmthRenewRsrcCurves_wind_CHINA

    L210.SmthRenewRsrcCurves_offshore_wind_CHINA %>%
      add_title("Supply curves of offshore wind resources") %>%
      add_units("maxSubResource: EJ; mid.price: $1975/GJ") %>%
      add_comments("Data from L210.SmthRenewRsrcCurves_offshore_wind added to all provinces") %>%
      add_legacy_name("NA") %>%
      add_precursors("L210.SmthRenewRsrcCurves_offshore_wind") ->
      L210.SmthRenewRsrcCurves_offshore_wind_CHINA

    L210.GrdRenewRsrcCurves_geo_CHINA %>%
      add_title("Geothermal resource curves in the provinces") %>%
      add_units("available: EJ; extractioncost: 1975$/GJ") %>%
      add_comments("China data from L210.GrdRenewRsrcCurves_geo filtered and written to all provinces") %>%
      add_legacy_name("NA") %>%
      add_precursors("L210.GrdRenewRsrcCurves_geo") ->
      L210.GrdRenewRsrcCurves_geo_CHINA

    L210.GrdRenewRsrcMax_geo_CHINA %>%
      add_title("Max sub resource for geothermal (placeholder)") %>%
      add_units("Unitless") %>%
      add_comments("L210.GrdRenewRsrcMax_geo filtered and written to relevant provinces, constant value used") %>%
      add_legacy_name("NA") %>%
      add_precursors("L210.GrdRenewRsrcMax_geo") ->
      L210.GrdRenewRsrcMax_geo_CHINA

    L210.SmthRenewRsrcCurvesGdpElast_roofPV_CHINA %>%
      add_title("Rooftop PV resource curves by province") %>%
      add_units("maxSubResource: EJ; mid.price = 1975$/GJ") %>%
      add_comments("China data from L210.GrdRenewRsrcCurves_geo filtered and written to all provinces") %>%
      add_legacy_name("NA") %>%
      add_precursors("L210.SmthRenewRsrcCurvesGdpElast_roofPV") ->
      L210.SmthRenewRsrcCurvesGdpElast_roofPV_CHINA

    L210.ResTechShrwt_CHINA %>%
      add_title("Technology share-weights for the renewable resources") %>%
      add_units("NA") %>%
      add_comments("Mostly just to provide a shell of a technology for the resource to use") %>%
      add_precursors("L210.SmthRenewRsrcCurves_wind") ->
      L210.ResTechShrwt_CHINA


    return_data(L210.RenewRsrc_CHINA, L210.RenewRsrcPrice_CHINA, L210.UnlimitRsrc_CHINA, L210.UnlimitRsrc_limestone_CHINA,
                L210.UnlimitRsrcPrice_CHINA, L210.UnlimitRsrcPrice_limestone_CHINA,
                L210.SmthRenewRsrcTechChange_CHINA, L210.SmthRenewRsrcCurves_wind_CHINA, L210.ResTechShrwt_CHINA,
                L210.SmthRenewRsrcTechChange_offshore_wind_CHINA, L210.SmthRenewRsrcCurves_offshore_wind_CHINA,
                L210.GrdRenewRsrcCurves_geo_CHINA, L210.GrdRenewRsrcMax_geo_CHINA, L210.SmthRenewRsrcCurvesGdpElast_roofPV_CHINA)
  } else {
    stop("Unknown command")
  }
}
