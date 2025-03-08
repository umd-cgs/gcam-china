# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_socioeconomics_SSP_xml
#'
#' Construct XML data structure for all the \code{socioeconomics_CHINA_[g]SSP[1-5].xml} files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{socioeconomics_CHINA_gSSP1.xml}, \code{socioeconomics_CHINA_gSSP2.xml}, \code{socioeconomics_CHINA_gSSP3.xml},
#' \code{socioeconomics_CHINA_gSSP4.xml}, \code{socioeconomics_CHINA_gSSP5.xml}, \code{socioeconomics_CHINA_SSP1.xml},
#' \code{socioeconomics_CHINA_SSP2.xml}, \code{socioeconomics_CHINA_SSP3.xml},
#' \code{socioeconomics_CHINA_SSP4.xml}, and \code{socioeconomics_CHINA_SSP5.xml}.
module_gcamchina_socioeconomics_SSP_xml <- function(command, ...) {

  SSP_NUMS <- 1:5

  if(command == driver.DECLARE_INPUTS) {
    return(c(paste0("L201.Pop_gSSP", SSP_NUMS),
             paste0("L201.Pop_SSP", SSP_NUMS),
             "L201.GDP_Scen",
             "L201.Pop_GCAMCHINA",
             "L201.GDP_GCAMCHINA",
             "L201.Pop_national_updated_China",
             "L201.GDP_national_updated_China"
))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "socioeconomics_CHINA_gSSP1.xml",
             XML = "socioeconomics_CHINA_gSSP2.xml",
             XML = "socioeconomics_CHINA_gSSP3.xml",
             XML = "socioeconomics_CHINA_gSSP4.xml",
             XML = "socioeconomics_CHINA_gSSP5.xml",
             XML = "socioeconomics_CHINA_SSP1.xml",
             XML = "socioeconomics_CHINA_SSP2.xml",
             XML = "socioeconomics_CHINA_SSP3.xml",
             XML = "socioeconomics_CHINA_SSP4.xml",
             XML = "socioeconomics_CHINA_SSP5.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

      socioeconomics_CHINA_gSSP1.xml <- socioeconomics_CHINA_gSSP2.xml <- socioeconomics_CHINA_gSSP3.xml <-
      socioeconomics_CHINA_gSSP4.xml <- socioeconomics_CHINA_gSSP5.xml <- socioeconomics_CHINA_SSP1.xml <-
      socioeconomics_CHINA_SSP2.xml <- socioeconomics_CHINA_SSP3.xml <- socioeconomics_CHINA_SSP4.xml <-
      socioeconomics_CHINA_SSP5.xml <- NULL  # silence package check notes

    # Load required inputs
    L201.GDP_Scen <- get_data(all_data, "L201.GDP_Scen")
    L201.Pop_GCAMCHINA <- get_data(all_data, "L201.Pop_GCAMCHINA")
    L201.GDP_GCAMCHINA <- get_data(all_data, "L201.GDP_GCAMCHINA")
    L201.Pop_national_updated_China <- get_data(all_data, "L201.Pop_national_updated_China")
    L201.GDP_national_updated_China <- get_data(all_data, "L201.GDP_national_updated_China")

    # Helper functions for data processing
    get_china_data <- function(data) {
      return(data %>% filter(region == "China"))
    }

    # Calculate shares for either population or GDP
    calculate_shares <- function(province_data, year, value_col) {
      total <- sum(province_data[[value_col]][province_data$year == year])
      return(province_data %>%
             filter(year == year) %>%
             mutate(share = .data[[value_col]]   / total))
    }

    # Distribute national values to provinces
    distribute_to_provinces <- function(national_data, shares_data, national_col, province_col) {
      provinces <- unique(shares_data$region)
      result <- data.frame()

      for(y in unique(national_data$year)) {
        if(y %in% shares_data$year) {
          year_shares <- calculate_shares(shares_data, y, province_col)
          national_value <- national_data[[national_col]][national_data$year == y]

          provincial_values <- year_shares %>%
            filter(year == y) %>%
            mutate(value = share * national_value) %>%
            select(region, year, value) %>%
            rename(!!national_col := value)

          result <- rbind(result, provincial_values)
        }
      }
      return(result)
    }

    # Process each SSP scenario
    for(g in c("g", "")) {
      for(ssp in SSP_NUMS) {
        gssp <- paste0(g, "SSP", ssp)

        # Get population data for this scenario
        popname <- paste0("L201.Pop_", gssp)
        pop_data <- get_data(all_data, popname)

        # Get China's population data
        china_pop <- get_china_data(pop_data)

        # Distribute population to provinces
        L201.Pop_CHINA_SSP <- distribute_to_provinces(
          china_pop,
          L201.Pop_GCAMCHINA,
          "totalPop",
          "totalPop"
        )

        # Get GDP data for this scenario
        china_gdp <- L201.GDP_Scen %>%
          filter(scenario == gssp, region == "China")

        # Distribute GDP to provinces
        L201.GDP_CHINA_Scen <- distribute_to_provinces(
          china_gdp,
          L201.GDP_GCAMCHINA,
          "GDP",
          "GDP"
        ) %>%
        mutate(scenario = gssp)

        # Create national summaries
        L201.Pop_national_updated <- L201.Pop_CHINA_SSP %>%
          group_by(year) %>%
          summarise(totalPop = sum(totalPop)) %>%
          mutate(region = "China")

        L201.GDP_national_updated <- L201.GDP_CHINA_Scen %>%
          group_by(year) %>%
          summarise(GDP = sum(GDP)) %>%
          mutate(region = "China")

        # Produce output XML
        xmlfn <- paste0("socioeconomics_CHINA_", gssp, ".xml")
        create_xml(xmlfn) %>%
          add_xml_data(L201.Pop_CHINA_SSP, "Pop") %>%
          add_xml_data(L201.GDP_CHINA_Scen %>%
                      filter(scenario == gssp) %>%
                      select(-scenario), "GDP") %>%
          add_xml_data(L201.GDP_national_updated, "GDP") %>%
          add_xml_data(L201.Pop_national_updated, "Pop")  %>%
          add_precursors("L201.GDP_Scen") %>%
          add_precursors("L201.Pop_GCAMCHINA", "L201.GDP_GCAMCHINA", "L201.Pop_national_updated_China", "L201.GDP_national_updated_China") ->
          x

        # Assign into environment
        assign(xmlfn, x)
      }
    }

    # Return all generated files
    return_data(socioeconomics_CHINA_gSSP1.xml, socioeconomics_CHINA_gSSP2.xml,
                socioeconomics_CHINA_gSSP3.xml, socioeconomics_CHINA_gSSP4.xml,
                socioeconomics_CHINA_gSSP5.xml, socioeconomics_CHINA_SSP1.xml,
                socioeconomics_CHINA_SSP2.xml, socioeconomics_CHINA_SSP3.xml,
                socioeconomics_CHINA_SSP4.xml, socioeconomics_CHINA_SSP5.xml)
  } else {
    stop("Unknown command")
  }
}
