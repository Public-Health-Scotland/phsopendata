#' Lists all available datasets
#'
#'

#'
#' @description
#' `list_datasets()` shows all of the datasets hosted on the PHS Open Data
#' Platform.
#'
#' @section Lifecycle:
#'
#'
#' `r lifecycle::badge("superseded")`
#'
#' `list_datasets()` has been superseded by `list_all_resources()`.
#' While `list_datasets()` only returns a list of dataset packages,
#' `list_all_resources()` provides a more comprehensive and flexible
#' interface for exploring the PHS Open Data platform. It returns both
#' datasets and their associated resources in a single tibble, and
#' supports filtering by dataset titles or resource names.

#'
#' @return A tibble.
#' @export
#'
#' @examplesIf isTRUE(length(curl::nslookup("www.opendata.nhs.scot", error = FALSE)) > 0L)
#' head(list_datasets())
#' @seealso [list_all_resources()]
list_datasets <- function() {
  # fetch the data
  content <- phs_GET("package_list", "")

  data_sets <- tibble::tibble(name = unlist(content$result))

  return(data_sets)
}
