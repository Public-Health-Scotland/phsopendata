#' Create fields query for GET
#'
#' @description Produces a comma separated list of column names for use in a GET request.
#'
#' @param col_select A character vector containing the names of desired columns/fields (e.g., c("Date", "Sex")).
#'
#' @return a character string
#' @noRd
#' @keywords internal
parse_col_select <- function(col_select) {
  if (is.null(col_select)) {
    return(NULL)
  }

  return(
    paste0(col_select, collapse = ",")
  )
}
