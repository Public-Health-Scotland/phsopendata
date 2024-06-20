#' Lists all available datasets
#'
#' `list_datasets()` shows all of the datasets hosted on the phs open data platform.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' head(list_datasets())
list_datasets <- function() {
  # fetch the data
  data_sets <- phs_GET("package_list", "")$result

  data_sets <- tibble::tibble("name" = unlist(data_sets))

  return(data_sets)}







