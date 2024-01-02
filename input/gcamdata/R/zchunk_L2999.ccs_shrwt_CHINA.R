# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_gcamchina_L2999.ccs_shrwt_CHINA
#'
#' Assign shareweight assumptions of CCS to subtechnologies by China province
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L9999.StubTechInterpOverwrite_CHINA}, \code{L9999.StubTechShrwt_CHINA}. The corresponding file in the
#' original data system was \code{L2999.ccs_shrwt_CHINA.R} (gcam-china level2).
#' @details Assign shareweight assumptions of CCS to subtechnologies by China province. Remove N fertilizer sector from Tibet(XZ) province.
#' @importFrom dplyr filter
#' @author YishengSun July 2019 / Yang Ou Dec 2023
module_gcamchina_L2999.ccs_shrwt_CHINA <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-china/province_names_mappings",
             FILE = "gcam-china/A999.StubTechShrwt_ccs",
             FILE = "gcam-china/A999.StubTechInterpOverwrite"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L9999.StubTechShrwt_CHINA",
             "L9999.StubTechInterpOverwrite_CHINA"))
  } else if(command == driver.MAKE) {


    all_data <- list(...)[[1]]

    # Silence package checks
    region <- supplysector <- subsector <- subsector <- stub.technology <- year <-
      share.weight <- apply.to <- from.year <- to.year <- interpolation.function <- subsector.name <- NULL

    # Load required inputs
    province_names_mappings <- get_data(all_data, "gcam-china/province_names_mappings", strip_attributes = T)
    StubTechShrwt_ccs <- get_data(all_data, "gcam-china/A999.StubTechShrwt_ccs", strip_attributes = T)
    StubTechInterpOverwrite <- get_data(all_data, "gcam-china/A999.StubTechInterpOverwrite", strip_attributes = T)

    # ===================================================
    # Apply new ccs shareweights to all provinces
    L9999.StubTechShrwt_CHINA <- StubTechShrwt_ccs %>%
      rename(subsector = subsector.name) %>%
      write_to_all_provinces(c(LEVEL2_DATA_NAMES[["StubTechShrwt"]]), gcamchina.PROVINCES_NOHKMC) %>%
      # remove Tibet(XZ) N fertilizer
       filter(!(region == "XZ" & supplysector == "N fertilizer"))

    L9999.StubTechInterpOverwrite_CHINA <- StubTechInterpOverwrite %>%
      write_to_all_provinces( c("region", "supplysector", "subsector", "stub.technology", "delete",
                                "apply.to", "from.year", "to.year", "interpolation.function"),
                              gcamchina.PROVINCES_NOHKMC) %>%
      # remove Tibet(XZ) N fertilizer
      filter(!(region == "XZ" & supplysector == "N fertilizer"))

    # ===================================================
    # Produce outputs
    L9999.StubTechShrwt_CHINA %>%
      add_title("Apply new ccs shareweights to all provinces") %>%
      add_units("Unitless") %>%
      add_comments("Assign shareweight assumptions of CCS to subtechnologies by China province") %>%
      add_comments("Remove N fertilizer sector from XZ province") %>%
      add_legacy_name("L9999.StubTechShrwt_CHINA") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/A999.StubTechShrwt_ccs") ->
      L9999.StubTechShrwt_CHINA

    L9999.StubTechInterpOverwrite_CHINA %>%
      add_title("Apply ccs shareweights overwrite methods to all provinces") %>%
      add_units("Unitless") %>%
      add_comments("Assign shareweight overwrite methods to China provinces") %>%
      add_comments("Remove N fertilizer sector from XZ province") %>%
      add_legacy_name("L9999.StubTechInterpOverwrite_CHINA") %>%
      add_precursors("gcam-china/province_names_mappings",
                     "gcam-china/A999.StubTechInterpOverwrite") ->
      L9999.StubTechInterpOverwrite_CHINA

    return_data(L9999.StubTechShrwt_CHINA,
                L9999.StubTechInterpOverwrite_CHINA)
  } else {
    stop("Unknown command")
  }
}


