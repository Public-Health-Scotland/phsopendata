#' Show all available datasets
#'
#' `list_datasets()` shows all of the datasets hosted on the phs open data platform.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' head(show_datasets())
#'
list_datasets <- function() {
  data_sets <- phs_GET("package_list", "")$result %>%
    unlist()

  return(data_sets)
}
