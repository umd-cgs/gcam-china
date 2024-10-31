# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_other_industry_incelas_SSP_CHINA_xml
#'
#' Construct XML data structures for all the \code{other_industry_incelas_SSP_CHINA.xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{other_industry_incelas_gcam3_china.xml}, \code{other_industry_incelas_ssp1_china.xml}, \code{other_industry_incelas_ssp2_china.xml}, \code{other_industry_incelas_ssp3_china.xml},
#' \code{other_industry_incelas_ssp4_china.xml}, \code{other_industry_incelas_ssp5_china.xml}, \code{other_industry_incelas_gssp1_china.xml}, \code{other_industry_incelas_gssp2_china.xml},
#' \code{other_industry_incelas_gssp3_china.xml}, \code{other_industry_incelas_gssp4_china.xml}, and \code{other_industry_incelas_gssp5_china.xml}.
module_energy_other_industry_incelas_SSP_CHINA_xml <- function(command, ...) {

  INCOME_ELASTICITY_INPUTS <- c("GCAM3",
                                paste0("gSSP", 1:5),
                                paste0("SSP", 1:5))

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
      paste("L232.IncomeElasticity_ind", tolower(INCOME_ELASTICITY_INPUTS), sep = "_")))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "other_industry_incelas_gcam3_china.xml",
             XML = "other_industry_incelas_gssp1_china.xml",
             XML = "other_industry_incelas_gssp2_china.xml",
             XML = "other_industry_incelas_gssp3_china.xml",
             XML = "other_industry_incelas_gssp4_china.xml",
             XML = "other_industry_incelas_gssp5_china.xml",
             XML = "other_industry_incelas_ssp1_china.xml",
             XML = "other_industry_incelas_ssp2_china.xml",
             XML = "other_industry_incelas_ssp3_china.xml",
             XML = "other_industry_incelas_ssp4_china.xml",
             XML = "other_industry_incelas_ssp5_china.xml"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    other_industry_incelas_gcam3_china.xml <- other_industry_incelas_ssp1_china.xml <- other_industry_incelas_ssp2_china.xml <- other_industry_incelas_ssp3_china.xml <-
      other_industry_incelas_ssp4_china.xml <- other_industry_incelas_ssp5_china.xml<- other_industry_incelas_gssp1_china.xml<- other_industry_incelas_gssp2_china.xml<-
      other_industry_incelas_gssp3_china.xml<- other_industry_incelas_gssp4_china.xml <- other_industry_incelas_gssp5_china.xml <- NULL

    all_data <- list(...)[[1]]

    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = T)

    # Loop through all the GCAM3, SSP, and gSSP objects and build the corresponding XML structure
    for(iei in INCOME_ELASTICITY_INPUTS) {
      data_obj <- paste0("L232.IncomeElasticity_ind_", tolower(iei))
      xmlfn <- paste0("other_industry_incelas_", tolower(iei), '_china.xml')

	  inclome_data = get_data(all_data, data_obj)

	  inclome_data %>%
	  filter(region ==  "China") %>%
	  write_to_all_provinces(names = c(LEVEL2_DATA_NAMES[["IncomeElasticity"]]),province = province_names_mappings$province)	  ->
	  inclome_data

      create_xml(xmlfn) %>%
        add_xml_data(inclome_data, "IncomeElasticity") %>%
        add_precursors(paste0("L232.IncomeElasticity_ind_", tolower(iei)),
                       "gcam-china/province_names_mappings") ->
        xml_obj


      # Assign output to output name
      assign(xmlfn, xml_obj)
    }


    return_data(other_industry_incelas_gcam3_china.xml,
                other_industry_incelas_ssp1_china.xml, other_industry_incelas_ssp2_china.xml, other_industry_incelas_ssp3_china.xml, other_industry_incelas_ssp4_china.xml, other_industry_incelas_ssp5_china.xml,
                other_industry_incelas_gssp1_china.xml, other_industry_incelas_gssp2_china.xml, other_industry_incelas_gssp3_china.xml, other_industry_incelas_gssp4_china.xml, other_industry_incelas_gssp5_china.xml)
  } else {
    stop("Unknown command")
  }
}
