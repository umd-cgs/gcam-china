# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L270.limits_CHINA
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
#' the generated outputs: \code{L270.CreditMkt_CHINA}, \code{L270.CreditInput_elecS_CHINA}, \code{L270.NegEmissBudgetMaxPrice_CHINA},
#' \code{paste0( "L270.NegEmissBudget_CHINA_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)) )}.
#' @details Add provinces to China market for GCAM policy constraints which enforce limits
#' to liquid feedstocks and the amount of subsidies given for net negative emissions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join filter
#' @author BY Jun 2020
module_gcamchina_L270.limits_CHINA <- function(command, ...) {
  negative_emiss_input_names <- paste0("L270.NegEmissBudget_", c("GCAM3", paste0("SSP", 1:5), paste0("gSSP", 1:5)) )
  negative_emiss_output_names <- sub('NegEmissBudget', 'NegEmissBudget_CHINA', negative_emiss_input_names)
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             "L270.CreditOutput",
             "L270.CreditMkt",
             "L270.NegEmissBudgetMaxPrice",
             negative_emiss_input_names))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L270.CreditMkt_CHINA",
             "L270.CreditOutput_CHINA",
             "L270.NegEmissBudgetMaxPrice_CHINA",
             # TODO: might just be easier to keep the scenarios in a single
             # table here and split when making XMLs but to match the old
             # data system we will split here
             negative_emiss_output_names))
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

    ret_data <- c("L270.CreditMkt_CHINA",
                  "L270.CreditOutput_CHINA",
                  "L270.NegEmissBudgetMaxPrice_CHINA")

    # Create the negative emissions GDP budget constraint limits

    # We will generate a bunch of tibbles for the negative emissions budgets for each scenario
    # and use assign() to save them to variables with names as L270.NegEmissBudget_[SCENARIO]
    # Note that since the call to assign() is in the for loop we must explicitly set the
    # environment to just outside of the loop:
    curr_env <- environment()
    for(i in seq_along(negative_emiss_input_names)) {
      curr_data <- get_data(all_data, negative_emiss_input_names[i], strip_attributes = T)
      curr_data %>%
        filter(region == gcamchina.REGION) %>%
        write_to_all_provinces(names(curr_data), gcamchina.PROVINCES_ALL) %>%
        add_title(paste0("The negative emissions budget in scenario ", negative_emiss_input_names[i])) %>%
        add_units("mil 1990$") %>%
        add_comments("The budget a market is willing to subsidize negative emissions") %>%
        add_precursors(negative_emiss_input_names[i]) %>%
        assign(negative_emiss_output_names[i], ., envir = curr_env)

      ret_data <- c(ret_data, negative_emiss_output_names[i])
    }

    # Call return_data but we need to jump through some hoops since we generated some of the
    # tibbles from the scenarios so we will generate the call to return_data
    ret_data %>%
      paste(collapse = ", ") %>%
      paste0("return_data(", ., ")") %>%
      parse(text = .) %>%
      eval()
  } else {
    stop("Unknown command")
  }
}
