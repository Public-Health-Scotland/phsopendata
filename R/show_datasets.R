#' Show all available datasets
#'
#' `show_datasets()`Shows all datasets available form phs open data platform.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' show_datasets()
#'
show_datasets <- function() {
  data_sets <- phs_GET("package_list", "")$result %>%
    unlist()

  return(data_sets)
}
