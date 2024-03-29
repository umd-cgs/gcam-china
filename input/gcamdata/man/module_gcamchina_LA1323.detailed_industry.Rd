% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zchunk_LA1323.Detailed_industry.R
\name{module_gcamchina_LA1323.detailed_industry}
\alias{module_gcamchina_LA1323.detailed_industry}
\title{module_gcamchina_LA1323.detailed_industry}
\usage{
module_gcamchina_LA1323.detailed_industry(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on /code{command}: either a vector of required inputs,
a vector of output names, or (if /code{command} is "MAKE") all
the generated outputs: /code{L1323.out_Mt_province_detailed_industry_Yh}, /code{L1323.IO_GJkg_province_detailed_industry_F_Yh}, /code{L1323.in_EJ_province_detailed_industry_F_Y}, /code{L1323.in_EJ_province_indnochp_F}. The corresponding file in the
}
\description{
To calculate national detailed_industry production, energy inputs and Input-output coefficients to provinces
}
\details{
The tables for detailed_industry production, i.e., out, and energy inputs, i.e., in, were calculated by applying province shares to national data.

The province shares were determined by the provinces' relative values of detailed_industry shipments.

The input-out coefficients were calculated by energy inputs divide outputs.
}
\author{
YangLiu Sep 2020
}
