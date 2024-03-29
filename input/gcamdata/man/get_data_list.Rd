% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-data.R
\name{get_data_list}
\alias{get_data_list}
\title{get_data_list}
\usage{
get_data_list(all_data, data_list, strip_attributes = FALSE, environ = NULL)
}
\arguments{
\item{all_data}{Data structure}

\item{data_list}{A character vector of data to load into the given environment}

\item{strip_attributes}{A logical vector which will be passed on to \code{get_data}.  The length
must be 1 (use the save logical for all values of data_list) or match the length of
\code{data_list} (one logical value for each data_list item).}

\item{environ}{The environment into which the data should be loaded.  If NULL (the default)
the caller's environment will be used.}
}
\description{
This function calls \code{get_data} for each data name in \code{data_list} and assigns
the resulting data into the given \code{environ} with the data name being used as the
variable name.  Note: for values in data_list "named" FILE the "basename" of the string
will be used as the variable name.
}
