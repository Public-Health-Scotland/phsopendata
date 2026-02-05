#' Lists all available datasets
#'
#' `list_datasets()` shows all of the datasets hosted on the PHS Open Data
#' Platform.
#'
#' @return A tibble.
#' @export
#'
#' @examplesIf isTRUE(length(curl::nslookup("www.opendata.nhs.scot", error = FALSE)) > 0L)
#' head(list_datasets())
list_datasets <- function() {
  # fetch the data
  content <- phs_GET("package_list", "")

  data_sets <- tibble::tibble(name = unlist(content$result))

  return(data_sets)
}
