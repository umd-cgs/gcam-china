# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L100.Socioeconomics_preliminary
#'
#' This chunk generates historical and future GDP and Population data by province
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L100.population_province_SSP}, \code{L100.gdp_province_SSP},
#' @details This chunk generated historical and future GDP and population data by province
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @import dplyr
#' @importFrom tidyr gather spread
#' @author MAW September 2020
module_gcamchina_L100.Socioeconomics_preliminary <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/China_Compendium_of_Statistics",
             FILE = "gcam-china/China_Statistics_Yearbook",
             FILE = "gcam-china/Population_Growth_Rates",
             FILE = "gcam-china/SSPS_Population",
             FILE = "gcam-china/GDP_raw",
             FILE = "gcam-china/GDP_deflator",
             FILE = "gcam-china/IMF_growth_rates",
             FILE = "gcam-china/SSPS_GDP"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L100.population_province_SSP",
             "L100.gdp_province_SSP"))
  } else if(command == driver.MAKE) {

    Region <- province.name <- Year <- GDP <- Population <- Sum <- Difference <- . <- `National Total` <- `Adjusted Population` <-
      SSP1 <- SSP2 <- SSP3 <- SSP4 <- SSP5 <- `SSP Growth Rate` <- `Growth Rate` <- `SSP National Projection` <-
      GDPperC_currentCNY <- `GDP deflator` <- GDPperC_constant2010CNY <- GDPperCadj <- GDPfinal <- GDP_deflator_CNY_WB <-
      National_GDP_perC <- GDPperCprojections <- GDPperC <- GDPbyCalcSum <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    # population data
    population_to_1999 <- get_data(all_data, "gcam-china/China_Compendium_of_Statistics" ) # the data in this CSV will be used for 1975 - 1999
    population_to_2019 <- get_data(all_data, "gcam-china/China_Statistics_Yearbook" ) # the data in this CSV will be used for 2000 - 2019
    population_growth_rate <- get_data(all_data, "gcam-china/Population_Growth_Rates" ) # the data in this CSV will be used for 2020 - 2100
    SSP_pop <- get_data(all_data, "gcam-china/SSPS_Population" ) # IIASA-WiC POP Region R32CHN

    # gdp data
    GDP_raw <- get_data(all_data, "gcam-china/GDP_raw" ) # CNKI data extractor (China Statistics Yearbook and China Compendium of Statistics (60-year statistics))
    GDP_deflator_raw <- get_data(all_data, "gcam-china/GDP_deflator" ) # https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=CN
    IMF_growth_rates <- get_data(all_data, "gcam-china/IMF_growth_rates" ) # IMF GDP per capita growth rates-World Economic Outlook (April 2020)
    SSP_gdp <- get_data(all_data, "gcam-china/SSPS_GDP" ) # OECD Env-Growth Region R32CHN

  # ------------------------------------------------------------------------------------------------------------------------
  # Process Population first -----------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------------------------------
    # 1. POPULATION
    # 1.1 1975 - 1999
    # For 1975 - 1999, the raw data comes from the China Compendium of Statistics
    # We are taking the provincial value from the (CCoS/sum of all provinces) * National Total * 10)
    # This gives us provincial totals in unit 1000 persons
    # First, make the data long and remove years 2000 and up
    population_to_1999 %>%
      filter( !grepl( "National Total|sum|Difference", Region ) ) %>%
      gather( Year, Population, -c( Region ) ) %>%
      mutate( Year = as.integer( Year ) ) %>%
      filter( Year < 2000 ) -> population_1999_long

    population_to_1999 %>%
      filter( grepl( "National Total", Region ) ) %>%
      gather( Year, "National Total" ) %>%
      mutate( Year = as.integer( Year ) ) %>%
      filter( Year < 2000 ) -> national_1999_long

    population_to_1999 %>%
      filter( grepl( "sum", Region ) ) %>%
      gather( Year, Sum ) %>%
      mutate( Year = as.integer( Year ) ) %>%
      filter( Year < 2000 ) -> sum_1999_long

    population_to_1999 %>%
      filter( grepl( "Difference", Region ) ) %>%
      gather( Year, Difference ) %>%
      mutate( Year = as.integer( Year ) ) %>%
      filter( Year < 2000 ) -> difference_1999_long

    # join the national, sum, and difference dataframes by year
    national_1999_long %>%
      left_join( sum_1999_long, by = "Year" ) %>%
      left_join( difference_1999_long, by = "Year" ) %>%
      mutate( `National Total` = as.numeric( `National Total` ),
              Difference = as.numeric( Difference ),
              Sum = as.numeric( Sum ) ) -> all_national_long

    # join the national data with provincial
    population_1999_long %>%
      left_join( all_national_long, by = "Year" ) -> population_1999_joined

    # create the adjusted population column
    population_1999_joined %>%
      mutate( "Adjusted Population" = ( Population/Sum ) * `National Total` * 10 ) -> population_1999_adjusted

    # remove unnecessary columns
    population_1999_adjusted %>%
      select( -c( Population, `National Total`, Sum, Difference) ) -> population_1999_adjusted_refined

    # make data wide again
    population_1999_adjusted_refined %>%
      spread( Year, `Adjusted Population` ) -> population_1999_final

    # 1.2 2000 - 2019
    # For 2000 - 2019, the raw data comes from the China Statistics Yearbook
    # We are taking the provincial value from the (CCoS/sum of all provinces) * National Total * 10)
    # This gives us provincial totals in unit 1000 persons
    # First, make the data long
    population_to_2019 %>%
      filter( !grepl( "National Total|sum|difference", Region ) ) %>%
      gather( Year, Population, -c( Region ) ) %>%
      mutate( Year = as.integer( Year ) ) -> population_2019_long

    population_to_2019 %>%
      filter( grepl( "National Total", Region ) ) %>%
      gather( Year, "National Total", -c( Region ) ) %>%
      mutate( Year = as.integer( Year ) ) %>%
      select( -c( Region ) ) -> national_2019_long

    population_to_2019 %>%
      filter( grepl( "sum", Region ) ) %>%
      gather( Year, Sum, -c( Region ) ) %>%
      mutate( Year = as.integer( Year ) ) %>%
      select( -c( Region ) ) -> sum_2019_long

    population_to_2019 %>%
      filter( grepl( "difference", Region ) ) %>%
      gather( Year, Difference, -c( Region ) ) %>%
      mutate( Year = as.integer( Year ) ) %>%
      select( -c( Region ) ) -> difference_2019_long

    # join the national, sum, and difference dataframes by year
    national_2019_long %>%
      left_join( sum_2019_long, by = "Year" ) %>%
      left_join( difference_2019_long, by = "Year" ) -> all_national_long

    # join the national data with provincial
    population_2019_long %>%
      left_join( all_national_long, by = "Year" ) -> population_2019_joined

    # create the adjusted population column
    population_2019_joined %>%
      mutate( "Adjusted Population" = ( Population/Sum ) * `National Total` * 10 ) -> population_2019_adjusted

    # remove unnecessary columns
    population_2019_adjusted %>%
      select( -c( Population, `National Total`, Sum, Difference) ) -> population_2019_adjusted_refined

    # make data wide again
    population_2019_adjusted_refined %>%
      spread( Year, `Adjusted Population` ) -> population_2019_final

    # 1.3 2020 - 2100
    # For 2020 - 2100, the raw data for future projections comes from the China Statistics Yearbook
    # 2020 - 2070 are calculated based on the growth rates csv
    # and 2071 - 2100 are calculated based on SSP growth rates
    # We need to make several long dataframes and join them to do calculations
    # The general formula is adjusted population for year = national SSP population (year) *
    # ((CSY province * 10 (year prior)) * (1 + growth rate (year))) / sum provinces (year)
    # We will do growth rates first
    population_growth_rate %>%
      gather( Year, "Growth Rate", -c( Region ) ) %>%
      mutate( Year = as.integer( Year ) ) -> growth_rate_long

    # We need a long dataframe for SSP estimated population
    # This is calculated using CSY National Total for the year prior * (1 + CSY National Total for 2013)
    # To update SSPs to a different scenario, change lines #137 and #138 to the scenario you want
    SSP_growth_rate <- SSP_pop %>%
      select( c( Year, SSP2 ) ) %>%
      rename( "SSP Growth Rate" = "SSP2" ) %>%
      mutate( `SSP Growth Rate` = as.numeric( `SSP Growth Rate` ),
              `SSP Growth Rate` = `SSP Growth Rate` / 100 )

    # Now we need to apply this to the National Total dataframe, where the previous year is used to, multiply by 10
    # First, join national with SSP growth
    SSP_growth_rate %>%
      left_join( national_2019_long, by = "Year" ) %>%
      filter( Year > 2018) %>%
      mutate( "SSP National Projection" = `National Total` * 10 ) %>%
      select( -c( `National Total` ) ) -> national_SSP_projection

    # SSP National Projection = Population of previous year * (1 + SSP Growth rate for that year)
    # Example: 2022 projection = 2021 population * (1 + 2022 growth rate)
    # Detect index
    index <- min( which( is.na( national_SSP_projection$`SSP National Projection` ) ) )
    # Loop
    for( i in index:dim( national_SSP_projection )[1] )
    {
      national_SSP_projection$`SSP National Projection`[i] <-
        ( national_SSP_projection$`SSP National Projection`[i-1] )*( 1 + national_SSP_projection$`SSP Growth Rate`[i] )
    }

    national_SSP_projection %>%
      select( -c( `SSP Growth Rate` ) ) %>%
      filter( Year > 2019 ) -> SSP_future_years

    # Compute other population projection for 2020 - 2070
    population_2019_long %>%
      mutate( Population = Population * 10 ) %>%
      filter( Year > 2018 ) -> population_2019_long_refined

    population_2019_long_refined %>%
      bind_rows( growth_rate_long ) -> population_growth_rate_long

    region_list <- c( "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", "Jilin",
                      "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", "Jiangxi",
                      "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan",
                      "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Shaanxi",
                      "Gansu", "Qinghai", "Ningxia", "Xinjiang" )

    for( region in region_list ){
      region_name <- paste0( region )

      population_growth_rate_long %>%
        filter( Region == region_name ) -> region_out

      # Detect index
      index <- min( which( is.na( region_out$Population ) ) )
      # Loop
      # Population = Population of previous year * (1 + Growth rate for that year)
      # Example: 2036 projection = 2035 population * (1 + 2036 growth rate)
      for( i in index:dim( region_out )[1] )
      {
       region_out$Population[i] <-
          ( region_out$Population[i-1] )*( 1 + region_out$`Growth Rate`[i] )
      }

      region_out %>%
        filter( Year > 2019 ) %>%
        select( -c( `Growth Rate` ) ) -> output

      assign( paste0( "projection_to_2070_", region_name ), output )
    }

    # row bind all of the 2070 dataframes
    # list the names of the dataframes for easier row binding
    projection_2070_list <- mget( ls( pattern = "^projection_to_2070_" ) )

    # row bind all of the dataframes in the list
    bind_rows( projection_2070_list ) -> projections_to_2070

    # Compute other population projection for 2071 - 2100
    projections_to_2070 %>%
      filter( Year > 2069 ) -> population_2070

    SSP_growth_rate %>%
      filter( Year > 2070 ) -> SSP_to_2100

    for( region in region_list ){
      region_name <- paste0( region )

      population_2070 %>%
        filter( Region == region_name) %>%
        bind_rows( SSP_to_2100 ) -> region_2100_out

      # Detect index
      index <- min( which( is.na( region_2100_out$Population ) ) )
      # Loop
      # Population = Population of previous year * (1 + Growth rate for that year)
      # Example: 2098 projection = 2097 population * (1 + 2098 growth rate)
      for( i in index:dim( region_2100_out )[1] )
      {
        region_2100_out$Population[i] <-
          ( region_2100_out$Population[i-1] )*( 1 + region_2100_out$`SSP Growth Rate`[i] )
      }

      region_2100_out %>%
        select( -c( `SSP Growth Rate` ) ) %>%
        mutate( "Region" = region_name ) %>%
        filter( Year > 2070) -> output

      assign( paste0( "projection_to_2100_", region_name ), output )
    }

    # row bind all of the 2070 dataframes
    # list the names of the dataframes for easier row binding
    projection_2100_list <- mget( ls( pattern = "^projection_to_2100_" ) )

    # row bind all of the dataframes in the list
    bind_rows( projection_2100_list ) -> projections_to_2100

    # Now we can continue processing years 2020 - 2100
    # Bind the two dataframes
    projections_to_2070 %>%
      bind_rows( projections_to_2100 ) -> future_year_projections

    # Now we need to make the data wide, before making it long again to get yearly provincial sums
    future_year_projections %>%
      spread( Year, Population ) -> future_year_wide

    # Add a sum row
    future_year_wide %>%
      bind_rows(summarise_all(., function(x) if(is.numeric(x)) sum(x) else "sum")) -> future_year_sum

    # Make one dataframe with sum in long form
    future_year_sum %>%
      filter( grepl( "sum", Region ) ) %>%
      gather( Year, Sum, -c( Region ) ) %>%
      select( -c( Region ) ) %>%
      mutate( `Year` = as.numeric( `Year` ) )-> future_year_sum_long

    # Join everything to do math
    future_year_projections %>%
      left_join( SSP_future_years, by = "Year" ) %>%
      left_join( future_year_sum_long, by = "Year" ) -> population_projections_toCalc_long

    # Calculate adjusted population, and remove unnecessary columns
    population_projections_toCalc_long %>%
      mutate( "Adjusted Population" = ( Population * `SSP National Projection` )/ Sum ) %>%
      select( -c( Sum, Population, `SSP National Projection` ) ) -> population_projections_long

    # make data wide again
    population_projections_long %>%
      spread( Year, `Adjusted Population` ) -> population_2100_final

    # Bind 1999 with 2019 and 2100
    population_1999_final %>%
      left_join( population_2019_final, by = "Region" ) %>%
      left_join( population_2100_final, by = "Region" ) %>%
      rename( province.name = Region ) -> population_final

    # ------------------------------------------------------------------------------------------------------------------------
    # Process GDP
    # ------------------------------------------------------------------------------------------------------------------------
    # set constant values
    GDP_deflator_2010 <- 87.60268
    CNY_USD_2010_exchange_rate <- 6.77027

    # 1. GDP
    # 1.1 1975-2019
    # Make the data long
    GDP_raw %>%
      mutate(across(where(is.factor), as.character)) %>%
      mutate(across(starts_with("X"), ~ gsub(",", "", .))) %>%
      mutate(across(starts_with("X"), ~ as.numeric(.))) %>%
      filter( !grepl( "National Total|sum|difference", province.name ) ) %>%
      gather( Year, GDP, -c( province.name ) ) %>%
      mutate( Year = as.integer( Year ) ) -> GDP_raw_long

    GDP_raw %>%
      mutate(across(where(is.factor), as.character)) %>%
      mutate(across(starts_with("X"), ~ gsub(",", "", .))) %>%
      mutate(across(starts_with("X"), ~ as.numeric(.))) %>%
      filter( grepl( "National Total", province.name ) ) %>%
      gather( Year, "National Total", -c( province.name ) ) %>%
      mutate( Year = as.integer( Year ) ) %>%
      select( -c( "province.name" ) ) -> GDP_total_long

    GDP_raw %>%
      mutate(across(where(is.factor), as.character)) %>%
      mutate(across(starts_with("X"), ~ gsub(",", "", .))) %>%
      mutate(across(starts_with("X"), ~ as.numeric(.))) %>%
      filter( grepl( "sum", province.name ) ) %>%
      gather( Year, Sum, -c( province.name ) ) %>%
      mutate( Year = as.integer( Year ) ) %>%
      select( -c( "province.name" ) ) -> GDP_sum_long

    GDP_raw %>%
      mutate(across(where(is.factor), as.character)) %>%
      mutate(across(starts_with("X"), ~ gsub(",", "", .))) %>%
      mutate(across(starts_with("X"), ~ as.numeric(.))) %>%
      filter( grepl( "difference", province.name ) ) %>%
      gather( Year, Difference, -c( province.name ) ) %>%
      mutate( Year = as.integer( Year ) ) %>%
      select( -c( "province.name" ) ) -> GDP_difference_long

    # We also need the population data in long form
    population_final %>%
      gather( Year, Population, -c( province.name ) ) %>%
      mutate( Year = as.integer( Year ) ) %>%
      filter( Year < 2020 ) -> population_2019_long

    # GDP deflator in format to join
    GDP_deflator_raw %>%
      select( c( "Year", "GDP deflator" ) ) -> GDP_deflator

    # join all dataframes together in long format
    GDP_raw_long %>%
      left_join( population_2019_long, by = c( "Year", "province.name" ) ) %>%
      left_join( GDP_total_long, by = "Year" ) %>%
      left_join( GDP_sum_long, by = "Year" ) %>%
      left_join( GDP_difference_long, by = "Year" ) %>%
      left_join( GDP_deflator, by = "Year" ) -> GDP_2019_long

    # ======================================================= calculations
    GDP_2019_long %>%
      mutate( "GDPperC_currentCNY" = (((( GDP / Sum ) * `National Total` ) * 100000000 ) / Population ) / 1000,
              "GDPperC_constant2010CNY" = GDPperC_currentCNY * ( GDP_deflator_2010 / `GDP deflator` ),
              "GDPperCadj" = GDPperC_constant2010CNY / CNY_USD_2010_exchange_rate,
              "GDPfinal" = ( GDPperCadj * Population ) / 1000 ) -> GDP_2019

    GDP_2019 %>%
      select( c( Year, GDPfinal, province.name ) ) %>%
      spread( Year, GDPfinal ) -> GDP_2019_final

    # 1.2
    # 2020-2024 (2020 & 2021 use IMF COVID adjusted growth rates. 2022 - 2024 use IMF growth rates)

    # Get the population data in long form
    population_final %>%
      gather( Year, Population, -c( province.name ) ) %>%
      mutate( Year = as.integer( Year ) ) %>%
      filter( Year > 2018, Year < 2025 ) -> population_long_2024

    population_final %>%
      bind_rows(summarise_all(., function(x) if(is.numeric(x)) sum(x) else "sum")) %>%
      filter( grepl( "sum", province.name ) ) %>%
      gather( Year, Sum, -c( province.name ) ) %>%
      select( -c( province.name ) ) %>%
      mutate( Year = as.numeric( Year ) )-> future_year_sum_long_2024

    population_long_2024 %>%
      left_join( future_year_sum_long_2024, by = "Year" ) %>%
      left_join( GDP_deflator, by = "Year" ) %>%
      left_join( GDP_total_long, by = "Year" ) %>%
      left_join( IMF_growth_rates, by = "Year" ) %>%
      mutate( "GDP_deflator_CNY_WB" = (( `National Total` / 10 ) *
                                                ( GDP_deflator_2010 / `GDP deflator` ) ) / CNY_USD_2010_exchange_rate ) -> GDP_deflator_CNY_WB_2024

    # Using IMF Growth Rates
    GDP_deflator_CNY_WB_2024 %>%
      mutate( `National_GDP_perC` = ( `GDP_deflator_CNY_WB` / `Sum` ) * 1000000 ) %>%
      arrange( province.name, Year ) -> GDP_2024_rates

    # Break into provincial dataframes for the next step to work
    for( region in region_list ){
      region_name <- paste0( region )

      GDP_2024_rates %>%
        filter( province.name == region_name ) -> GDP_2024

    # Detect index
    index <- min( which( is.na( GDP_2024$National_GDP_perC ) ) )
    # Loop
    # GDP = GDP of previous year * (1 + Growth rate for that year)
    # Example: 2098 projection = 2097 GDP * (1 + 2098 growth rate)
    for( i in index:dim( GDP_2024 )[1] )
    {
      GDP_2024$National_GDP_perC[i] <-
        ( GDP_2024$National_GDP_perC[i-1] )*( 1 + GDP_2024$`IMF growth rate`[i] )

    GDP_2024 %>%
      filter( Year > 2018 ) %>%
      mutate( province.name = as.character( province.name ) ) -> output

    assign( paste0( "GDP_to_2024_", region_name ), output )
    }
}

    # row bind all of the GDP 2024 dataframes
    # list the names of the dataframes for easier row binding
    projection_GDP_2024_list <- mget( ls( pattern = "^GDP_to_2024_" ) )

    # row bind all of the dataframes in the list
    bind_rows( projection_GDP_2024_list ) -> National_GDP_perC_2024

    # 1.3
    # Now we have to process some 2025-2110 data, because we need those values to calculate provincial growth rates
    # Make a new dataframe that has all provinces, and years 2020-2100, 2110 for each province
    for( region in region_list ) {
      output <- data.frame( province.name = region,
                            Year = 2020:2110 )

      assign( paste0( "GDP_2110_", region ), output )
    }

    # row bind all of the GDP 2100 dataframes
    # list the names of the dataframes for easier row binding
    GDP_2110_list <- mget( ls( pattern = "^GDP_2110_" ) )

    # row bind all of the dataframes in the list
    bind_rows( GDP_2110_list ) -> province_2110

    # change SSP here to the scenario you want
    province_2110 %>%
      left_join( SSP_gdp, by = "Year" ) %>%
      select( c( province.name, Year, SSP2 ) ) %>%
      rename( "SSP Growth Rate" = "SSP2" ) %>%
      mutate( `SSP Growth Rate` = `SSP Growth Rate` / 100 ) %>%
      left_join( National_GDP_perC_2024, by = c( "Year", "province.name" ) ) -> SSP_joined

    SSP_joined %>%
      filter( Year > 2023, Year < 2101 ) %>%
      select( c( "province.name", "Year", "SSP Growth Rate", "National_GDP_perC")) -> SSP_2100

    # 2110 uses the same growth rate as 2100
    SSP_joined %>%
      filter( Year == 2110 ) %>%
      select( c( "province.name", "Year", "SSP Growth Rate", "National_GDP_perC")) -> SSP_2110

    # Using SSP Growth Rates
    # Break into provincial dataframes for the next step to work
    for( region in region_list ){
      region_name <- paste0( region )

      SSP_2100 %>%
        filter( province.name == region_name ) -> GDP_SSP_to_2100

      # Detect index
      index <- min( which( is.na( GDP_SSP_to_2100$National_GDP_perC ) ) )
      # Loop
      # GDP = GDP of previous year * (1 + Growth rate for that year)
      # Example: 2098 projection = 2097 GDP * (1 + 2098 growth rate)
      for( i in index:dim( GDP_SSP_to_2100 )[1] )
      {
        GDP_SSP_to_2100$National_GDP_perC[i] <-
          ( GDP_SSP_to_2100$National_GDP_perC[i-1] )*( 1 + GDP_SSP_to_2100$`SSP Growth Rate`[i] )

        assign( paste0( "GDP_to_2100_", region_name ), GDP_SSP_to_2100 )
      }
  }

    # row bind all of the GDP 2100 dataframes
    # list the names of the dataframes for easier row binding
    projection_GDP_SSP_2100_list <- mget( ls( pattern = "^GDP_to_2100_" ) )

    # row bind all of the dataframes in the list
    bind_rows( projection_GDP_SSP_2100_list ) -> SSP_2100_GDP

    # 2110 gets its values from 2100, so we need a dataframe that has just 2100 and 2110
    SSP_2100_GDP %>%
      filter( Year == 2100 ) %>%
      bind_rows( SSP_2110 ) -> SSP_bound

    # Break into provincial dataframes for the next step to work
    for( region in region_list ){
      region_name <- paste0( region )

      SSP_bound %>%
        filter( province.name == region_name ) -> GDP_SSP_2100_2110

      # Detect index
      index <- min( which( is.na( GDP_SSP_2100_2110$National_GDP_perC ) ) )
      # Loop
      # GDP = GDP of 2100 * (1 + Growth rate for that year)
      # Example: 2110 projection = 2100 GDP * (1 + 2110 growth rate)^10
      for( i in index:dim( GDP_SSP_2100_2110 )[1] )
      {
        GDP_SSP_2100_2110$National_GDP_perC[i] <-
          ( GDP_SSP_2100_2110$National_GDP_perC[i-1] )*(( 1 + GDP_SSP_2100_2110$`SSP Growth Rate`[i] )^10 )

        assign( paste0( "GDP_2100_to_2110_", region_name ), GDP_SSP_2100_2110 )
      }
    }

    # row bind all of the GDP 2100/2110 dataframes
    # list the names of the dataframes for easier row binding
    projection_GDP_SSP_2110_list <- mget( ls( pattern = "^GDP_2100_to_2110_" ) )

    # row bind all of the dataframes in the list
    bind_rows( projection_GDP_SSP_2110_list ) -> SSP_2110_GDP_bound

    SSP_2110_GDP_bound %>%
      fill( National_GDP_perC ) %>%
      filter( Year == 2110 ) -> SSP_2110_GDP

    SSP_2100_GDP %>%
      bind_rows( SSP_2110_GDP ) %>%
      filter( Year > 2024 ) %>%
      select( -c( `SSP Growth Rate` ) ) -> National_GDP_perC_2025_2110

    # Now back to processing 2020-2024
    # Get growth rates from 2019 GDPperCadj and 2110 National_GDP_perC
    GDP_2019 %>%
      filter( Year == 2019 ) %>%
      rename( "GDPperCadj_2019" = `GDPperCadj` ) %>%
      select( c( province.name, GDPperCadj_2019 ) ) -> GDPperCadj_2019

    National_GDP_perC_2025_2110 %>%
      filter( Year == 2110 ) %>%
      rename( "National_GDP_perC_2110" = `National_GDP_perC` ) %>%
      select( -c( Year ) ) -> National_GDP_perC_2110

    GDPperCadj_2019 %>%
      left_join( National_GDP_perC_2110, by = "province.name" ) %>%
      mutate( "Growth Rate" = ( ( National_GDP_perC_2110 / GDPperCadj_2019 ) ^ ( 1 / ( 2110 - 2019 ) ) ) - 1 )-> growth_rate_calc

    growth_rate_calc %>%
      mutate( "GDPperCprojections" = GDPperCadj_2019 * ( 1 + `Growth Rate` ),
              "Year" = "2020",
              Year = as.numeric( Year ) ) %>%
      select( -c( GDPperCadj_2019, National_GDP_perC_2110, `Growth Rate` ) ) -> GDPperCprojections_2020

    growth_rate_calc %>%
      select( c( province.name, `Growth Rate` ) ) -> growth_rate

    # Combine 2020-2024 data with 2025-2110 data, and clean up the dataframe by removing unnecessary years and columns
    National_GDP_perC_2024 %>%
      bind_rows( National_GDP_perC_2025_2110 ) %>%
      arrange( province.name ) %>%
      left_join( growth_rate, by = "province.name" ) %>%
      left_join( GDPperCprojections_2020, by = c( "Year", "province.name" ) ) %>%
      filter( Year > 2019, Year < 2101 ) %>%
      select( c( province.name, Year, National_GDP_perC, `Growth Rate`, GDPperCprojections ) ) -> year_2024_2100

      # Break into provincial dataframes for the next step to work
      for( region in region_list ){
        region_name <- paste0( region )

        year_2024_2100 %>%
          filter( province.name == region_name ) -> GDP_2024_to_2100

        # Detect index
        index <- min( which( is.na( GDP_2024_to_2100$GDPperCprojections ) ) )
        # Loop
        # GDP = GDP of previous year * (1 + Growth rate for that year)
        # Example: 2098 projection = 2097 GDP * (1 + 2098 growth rate)
        for( i in index:dim( GDP_2024_to_2100 )[1] )
        {
          GDP_2024_to_2100$GDPperCprojections[i] <-
            ( GDP_2024_to_2100$GDPperCprojections[i-1] )*( 1 + GDP_2024_to_2100$`Growth Rate`[i] )

          GDP_2024_to_2100 %>%
            rename( "GDPperC" = GDPperCprojections ) %>%
            select( -c( `Growth Rate` ) ) -> output

          assign( paste0( "GDP_2024_to_2100_", region_name ), output )
        }
      }

    # row bind all of the GDP 2024 dataframes
    # list the names of the dataframes for easier row binding
    projection_GDP_2024_2100_list <- mget( ls( pattern = "^GDP_2024_to_2100_" ) )

    # row bind all of the dataframes in the list
    bind_rows( projection_GDP_2024_2100_list ) -> GDPperC_2100

    # Get the population data in long format for 2020-2100
    population_final %>%
      gather( Year, Population, -c( province.name ) ) %>%
      mutate( Year = as.integer( Year ) ) %>%
      filter( Year > 2019 ) -> population_long_2100

    population_final %>%
      bind_rows( summarise_all(., function(x) if(is.numeric(x)) sum(x) else "sum") ) %>%
      filter( grepl( "sum", province.name ) ) %>%
      gather( Year, Sum, -c( province.name ) ) %>%
      select( -c( province.name ) ) %>%
      mutate( Year = as.numeric( Year ) )-> future_year_sum_long_2100

    population_long_2100 %>%
      left_join( future_year_sum_long_2100, by = "Year" ) -> population_2100

    # Join population dataframe and GDP dataframe, calculate GDP_by_calc
    population_2100 %>%
      left_join( GDPperC_2100, by = c( "Year", "province.name" ) ) %>%
      mutate( "GDPbyCalc" = ( GDPperC * Population ) / 1000 ) -> GDPbyCalc

    # Get yearly sum of GDPbyCalc in a new dataframe, then join back with other columns for final calculations
    GDPbyCalc %>%
      select( c( province.name, Year, GDPbyCalc ) ) %>%
      spread( Year, GDPbyCalc ) %>%
      bind_rows(summarise_all(., function(x) if(is.numeric(x)) sum(x) else "sum")) %>%
      filter( grepl( "sum", province.name ) ) %>%
      gather( Year, GDPbyCalcSum, -c( province.name ) ) %>%
      select( -c( province.name ) ) %>%
      mutate( Year = as.numeric( Year ) )-> GDPbyCalc_sum

    GDPbyCalc %>%
      left_join( GDPbyCalc_sum, by = "Year" ) %>%
      mutate( "GDPperCadj" = GDPbyCalc / GDPbyCalcSum * National_GDP_perC * Sum / Population,
              "GDPfinal" = ( GDPperCadj * Population ) / 1000 ) %>%
      select( c( province.name, Year, GDPfinal ) ) %>%
      spread( Year, GDPfinal ) -> GDP_2100_final

    # Join GDP 1975-2019 with 2020-2100
    GDP_2019_final %>%
      left_join( GDP_2100_final, by = "province.name" ) -> GDP_final


    # Produce outputs
    population_final %>%
      add_title("Future and historical population by province") %>%
      add_units("1000 persons") %>%
      add_comments("Created using SSP2 growth rates") %>%
      add_legacy_name("population_thous_province") %>%
      add_precursors("gcam-china/China_Compendium_of_Statistics",
                     "gcam-china/China_Statistics_Yearbook",
                     "gcam-china/Population_Growth_Rates",
                     "gcam-china/SSPS_Population") -> L100.population_province_SSP

    GDP_final %>%
      add_title("Future and historical GDP by province") %>%
      add_units("million 2010 USD") %>%
      add_comments("Created using SSP2 growth rates") %>%
      add_comments("Provincial GDP by historical and future years are calculated from GDP per capita and population") %>%
      add_legacy_name("gdp_mil10usd_province") %>%
      add_precursors("gcam-china/GDP_raw",
                     "gcam-china/GDP_deflator",
                     "gcam-china/IMF_growth_rates",
                     "gcam-china/SSPS_GDP") -> L100.gdp_province_SSP

    return_data(L100.population_province_SSP, L100.gdp_province_SSP)
  } else {
    stop("Unknown command")
  }
}
