# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L270.limits
#'
#' Add the provinces to the China market in each of the L270 limits polices.  In
#' particular to limit the fraction of liquid feedstocks and inputs to electricity
#' generation which can come from sources other than crude oil.  Constrain the
#' total amount of subsidy as a fraction of GDP which an economy is will to give
#' to have net negative emissions.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L270.CreditMkt_CHINA},\code{L270.CreditOutput_CHINA}, \code{L270.CreditInput_elecS_CHINA}, \code{L270.NegEmissBudgetMaxPrice_CHINA},
#' \code{paste0( "L270.NegEmissBudget_CHINA )}.
#' @details Add provinces to China market for GCAM policy constraints which enforce limits
#' to liquid feedstocks and the amount of subsidies given for net negative emissions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join filter
#' @author BY Jun 2020
module_gcamchina_L270.limits <- function(command, ...) {

  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             "L270.CreditOutput",
             "L270.CreditMkt",
             "L270.NegEmissBudgetMaxPrice",
             "L270.NegEmissBudget"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L270.CreditMkt_CHINA",
             "L270.CreditOutput_CHINA",
             "L270.NegEmissBudgetMaxPrice_CHINA",
			 "L270.NegEmissBudget_CHINA"))
  } else if(command == driver.MAKE) {

    value <- subsector <- supplysector <- year <- GCAM_region_ID <- sector.name <-
      region <- scenario <- constraint <- . <- subsector_1 <- Electric.sector <-
      subsector.name<- Electric.sector.technology <- minicam.energy.input <-
      coefficient<- NULL # silence package check notes

    all_data <- list(...)[[1]]

    # Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = T)
    L270.CreditMkt <- get_data(all_data, "L270.CreditMkt", strip_attributes = T)
    L270.CreditOutput <- get_data(all_data, "L270.CreditOutput", strip_attributes = T)
    L270.NegEmissBudgetMaxPrice <- get_data(all_data, "L270.NegEmissBudgetMaxPrice", strip_attributes = T)
	L270.NegEmissBudget <- get_data(all_data, "L270.NegEmissBudget", strip_attributes = T)

    # ===================================================
    # Data Processing

    L270.CreditMkt %>%
      filter(region == gcamchina.REGION) %>%
      write_to_all_provinces(names(L270.CreditMkt), gcamchina.PROVINCES_ALL) ->
      L270.CreditMkt_CHINA

    L270.CreditOutput %>%
      mutate(sector.name = "oil refining",
             subsector.name = "oil refining") ->
      L270.CreditOutput_CHINA

    L270.NegEmissBudgetMaxPrice %>%
      filter(region == gcamchina.REGION) %>%
      write_to_all_provinces(names(L270.NegEmissBudgetMaxPrice), gcamchina.PROVINCES_ALL) ->
      L270.NegEmissBudgetMaxPrice_CHINA

    L270.NegEmissBudget %>%
      filter(region == gcamchina.REGION) %>%
      # market is unchanged so as to share the USA policy
      write_to_all_provinces(names(L270.NegEmissBudget), gcamchina.PROVINCES_ALL) ->
      L270.NegEmissBudget_CHINA

    # ===================================================
    # Produce outputs

    L270.CreditMkt_CHINA %>%
      add_title("Add provinces to the oil-credits RES market") %>%
      add_units("NA") %>%
      add_comments("Boiler plate and units for creating the actual") %>%
      add_comments("market for balancing oil-credits") %>%
      add_precursors("gcam-china/province_names_mappings", "L270.CreditMkt") ->
      L270.CreditMkt_CHINA

    L270.CreditOutput_CHINA %>%
      add_title("Secondary output to add oil refining output to oil-credits market in GCAM-CHINA") %>%
      add_units("NA") %>%
      add_comments("The secondary output from L270.CreditOutput does not suffice in ") %>%
      add_comments("GCAM-CHINA because we renamed the sector / subsector thus the") %>%
      add_comments("global tech does not match.") %>%
      add_precursors("L270.CreditOutput") ->
      L270.CreditOutput_CHINA

    L270.NegEmissBudgetMaxPrice_CHINA %>%
      add_title("A hint for the solver for what the max price of this market is") %>%
      add_units("%") %>%
      add_comments("This value is just used to give the solver a hint of the") %>%
      add_comments("range of prices which are valid.  For the negative emissions") %>%
      add_comments("budget constraint the price is a fraction from 0 to 1") %>%
      add_precursors("L270.NegEmissBudgetMaxPrice") ->
      L270.NegEmissBudgetMaxPrice_CHINA

    L270.NegEmissBudget_CHINA %>%
      add_title("Sets up the negative emissions budget RES market") %>%
      add_units("NA") %>%
      add_comments("Sets up the RES constraint market including boiler plate such") %>%
      add_comments("as the policy name and market as well as unit strings") %>%
      add_precursors("L270.NegEmissBudget") ->
      L270.NegEmissBudget_CHINA

    return_data(L270.CreditMkt_CHINA,
                L270.CreditOutput_CHINA,
                L270.NegEmissBudgetMaxPrice_CHINA,
                L270.NegEmissBudget_CHINA)
  } else {
    stop("Unknown command")
  }
}
