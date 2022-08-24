#' Create fields query for GET
#' @description Produces a comma separated list
#'
#' @param col_select a character vector identifying the columns to select.
#' @return a character string
parse_col_select <- function(col_select) {
  if (is.null(col_select)) {
    return(NULL)
  }

  return(
    paste0(col_select, collapse = ",")
  )
}
