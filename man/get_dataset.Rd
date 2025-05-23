% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_dataset.R
\name{get_dataset}
\alias{get_dataset}
\title{Get Open Data resources from a dataset}
\usage{
get_dataset(
  dataset_name,
  max_resources = NULL,
  rows = NULL,
  row_filters = NULL,
  col_select = NULL,
  include_context = FALSE
)
}
\arguments{
\item{dataset_name}{name of the dataset as found on
\href{https://www.opendata.nhs.scot/}{NHS Open Data platform}}

\item{max_resources}{(optional) the maximum number of resources
to return, use for testing code,
it will return the n latest resources}

\item{rows}{(optional) specify the max number of rows
to return for each resource.}

\item{row_filters}{(optional) a named list or vector that specifies values of
columns/fields to keep.
e.g. list(Date = 20220216, Sex = "Female").}

\item{col_select}{(optional) a character vector containing the names of
desired columns/fields.
e.g. c("Date", "Sex").}

\item{include_context}{(optional) If \code{TRUE} additional information about the
resource will be added as columns to the data, including the resource ID, the
resource name, the creation date and the last modified/updated date.}
}
\value{
a \link[tibble:tibble-package]{tibble} with the data
}
\description{
Get Open Data resources from a dataset
}
\examples{
get_dataset("gp-practice-populations",
  max_resources = 2, rows = 10
)
}
\seealso{
\code{\link[=get_resource]{get_resource()}} for downloading a single resource
from a dataset.
}
