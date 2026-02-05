#' Determines if the datastore dump should be used
#'
#' @param query a list of items to query
#' @param rows Integer(1) or NULL. Number of rows requested.
#'
#' @return TRUE to use the datastore dump; FALSE to use the standard endpoint.#' @keywords internal
#' @noRd
use_dump_check <- function(query, rows) {
  null_query <- purrr::every(list(query$q, query$filter, query$fields), is.null)
  null_rows <- is.null(rows)

  if (null_query && null_rows) {
    # No filters specified - Use dump
    return(TRUE)
  } else if (null_rows) {
    # Filters specified, but no row limit - Don't use dump
    return(FALSE)
  } else if (rows > 99999L) {
    if (null_query) {
      # No filters specified but rows over 99999 requested. - Use dump
      cli::cli_warn(c(
        "Getting all rows of resource.",
        i = "All rows will be returned if you
      request over 99,999 rows of data.",
        i = "You set {.var rows} to
      {format(rows, big.mark = ',', scientific = FALSE)}"
      ))
      return(TRUE)
    } else {
      # Filters specified and rows over 99999 requested. - Use dump
      cli::cli_warn(c(
        "Invalid combination of {.var rows}, {.var row_filters}
      and/or {.var col_select}.",
        x = "Can't request over 99,999 rows of a resource
      AND query its rows/columns.",
        i = "ALL rows and columns of the resource will be downloaded."
      ))
      return(TRUE)
    }
  }

  # Normal query with filters and/or small num of rows - Don't use dump
  return(FALSE)
}
