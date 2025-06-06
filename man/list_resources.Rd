% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_resources.R
\name{list_resources}
\alias{list_resources}
\title{Lists all available resources for a dataset}
\usage{
list_resources(dataset_name)
}
\arguments{
\item{dataset_name}{Name of the dataset as found on \href{https://www.opendata.nhs.scot/}{NHS Open Data platform} (character).}
}
\value{
a \link[tibble:tibble-package]{tibble} with the data
}
\description{
\code{list_resources()} returns all of the resources associated
with a dataset
}
\examples{
list_resources("weekly-accident-and-emergency-activity-and-waiting-times")
}
