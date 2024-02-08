# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_iron_steel_incelas_SSP_CHINA_xml
#'
#' Construct XML data structures for all the \code{iron_steel_incelas_SSP_CHINA.xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{iron_steel_incelas_gcam3_china.xml}, \code{iron_steel_incelas_ssp1_china.xml}, \code{iron_steel_incelas_ssp2_china.xml}, \code{iron_steel_incelas_ssp3_china.xml},
#' \code{iron_steel_incelas_ssp4_china.xml}, \code{iron_steel_incelas_ssp5_china.xml}, \code{iron_steel_incelas_gssp1_china.xml}, \code{iron_steel_incelas_gssp2_china.xml},
#' \code{iron_steel_incelas_gssp3_china.xml}, \code{iron_steel_incelas_gssp4_china.xml}, and \code{iron_steel_incelas_gssp5_china.xml}.
module_energy_batch_iron_steel_incelas_SSP_CHINA_xml <- function(command, ...) {

  INCOME_ELASTICITY_INPUTS <- c("GCAM3",
                                paste0("gSSP", 1:5),
                                paste0("SSP", 1:5))

  if(command == driver.DECLARE_INPUTS) {
    return(c("L2323.IncomeElasticity_detailed_industry",
			paste("L2323.iron_steel_incelas", tolower(INCOME_ELASTICITY_INPUTS), sep = "_")))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "iron_steel_incelas_gcam3_china.xml",
             XML = "iron_steel_incelas_gssp1_china.xml",
             XML = "iron_steel_incelas_gssp2_china.xml",
             XML = "iron_steel_incelas_gssp3_china.xml",
             XML = "iron_steel_incelas_gssp4_china.xml",
             XML = "iron_steel_incelas_gssp5_china.xml",
             XML = "iron_steel_incelas_ssp1_china.xml",
             XML = "iron_steel_incelas_ssp2_china.xml",
             XML = "iron_steel_incelas_ssp3_china.xml",
             XML = "iron_steel_incelas_ssp4_china.xml",
             XML = "iron_steel_incelas_ssp5_china.xml"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    iron_steel_incelas_gcam3_china.xml <- iron_steel_incelas_ssp1_china.xml <- iron_steel_incelas_ssp2_china.xml <- iron_steel_incelas_ssp3_china.xml <-
      iron_steel_incelas_ssp4_china.xml <- iron_steel_incelas_ssp5_china.xml<- iron_steel_incelas_gssp1_china.xml<- iron_steel_incelas_gssp2_china.xml<-
      iron_steel_incelas_gssp3_china.xml<- iron_steel_incelas_gssp4_china.xml <- iron_steel_incelas_gssp5_china.xml <- NULL

    all_data <- list(...)[[1]]
	
	# Load required inputs
	L2323.IncomeElasticity_detailed_industry <- get_data(all_data, "L2323.IncomeElasticity_detailed_industry")

	L2323.IncomeElasticity_detailed_industry %>%
	 filter(energy.final.demand ==  "iron and steel") %>%
	 filter(year ==  c(2020)) ->
    L2323.IncomeElasticity_detailed_industry
  
    # Loop through all the GCAM3, SSP, and gSSP objects and build the corresponding XML structure
    for(iei in INCOME_ELASTICITY_INPUTS) {
      data_obj <- paste0("L2323.iron_steel_incelas_", tolower(iei))
      xmlfn <- paste0("iron_steel_incelas_",tolower(iei), '_china.xml')
	  
	  inclome_data = get_data(all_data, data_obj)
	  inclome_data %>%
	  filter(region ==  "China") %>%
	  filter(energy.final.demand ==  "regional iron and steel") %>%
	  mutate(energy.final.demand = "iron and steel") %>%
	  mutate(income.elasticity = if_else(year  == 2020, L2323.IncomeElasticity_detailed_industry$income.elasticity, income.elasticity)) ->
	  inclome_data
	  
      create_xml(xmlfn) %>%
        add_xml_data(inclome_data, "IncomeElasticity") %>%
        add_precursors("L2323.IncomeElasticity_detailed_industry",paste0("L2323.iron_steel_incelas_", tolower(iei))) ->
        xml_obj

      # Assign output to output name
      assign(xmlfn, xml_obj)
    }

    return_data(iron_steel_incelas_gcam3_china.xml,
                iron_steel_incelas_ssp1_china.xml, iron_steel_incelas_ssp2_china.xml, iron_steel_incelas_ssp3_china.xml, iron_steel_incelas_ssp4_china.xml, iron_steel_incelas_ssp5_china.xml,
                iron_steel_incelas_gssp1_china.xml, iron_steel_incelas_gssp2_china.xml, iron_steel_incelas_gssp3_china.xml, iron_steel_incelas_gssp4_china.xml, iron_steel_incelas_gssp5_china.xml)
  } else {
    stop("Unknown command")
  }
}
