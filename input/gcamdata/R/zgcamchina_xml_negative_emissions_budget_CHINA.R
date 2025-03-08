# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_negative_emissions_budget_xml
#'
#' Construct XML data structure for \code{"negative_emissions_budget_CHINA_.xml"}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{"negative_emissions_budget_CHINA.xml"}.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr spread
#' @importFrom stats weighted.mean
#' @author YangLiu August 2024

module_gcamchina_negative_emissions_budget_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L270.NegEmissBudget_CHINA",
    		 "L270.NegEmissBudgetMaxPrice_CHINA"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("XML" = "negative_emissions_budget_CHINA.xml"))
  } else if(command == driver.MAKE) {

    . <- NULL # silence package check note

    all_data <- list(...)[[1]]

    # Load required inputs
    L270.NegEmissBudgetMaxPrice_CHINA <- get_data(all_data, "L270.NegEmissBudgetMaxPrice_CHINA")
    L270.NegEmissBudget_CHINA <- get_data(all_data, "L270.NegEmissBudget_CHINA")

    # ===================================================

    create_xml("negative_emissions_budget_CHINA.xml") %>%
      add_xml_data(L270.NegEmissBudget_CHINA, "PortfolioStd") %>%
	  add_xml_data(L270.NegEmissBudgetMaxPrice_CHINA, "PortfolioStdMaxPrice") %>%
      add_precursors("L270.NegEmissBudget_CHINA","L270.NegEmissBudgetMaxPrice_CHINA") ->
      negative_emissions_budget_CHINA.xml

	return_data(negative_emissions_budget_CHINA.xml)
  } else {
    stop("Unknown command")
  }
}
