#' Determines if the datastore dump should be used
#'
#' @param query a list of items to query
#' @param rows the number of rows user is requesting
#'
#' @return a logical value. TRUE indicates that the dump should be used
#' @keywords internal
#' @noRd
use_dump_check <- function(query, rows) {
  # if row input is > 99999 or NULL
  # or all queries (inc. rows) are null
  # then use GET datastore_dump
  no_rows <- is.null(rows)
  if (no_rows) {
    use_dump <- is.null(c(query$q, query$filter, query$fields, rows))
  } else {
    use_dump <- rows > 99999 ||
      is.null(c(query$q, query$filter, query$fields, rows))
  }

  # warn users that dump will be used,
  # if user queried the data
  queried <- !is.null(query$q) ||
    !is.null(query$filter) ||
    !is.null(query$fields)

  if (queried && use_dump) {
    cli::cli_warn(c(
      "Invalid combination of {.var rows}, {.var row_filters}
      and/or {.var col_select}.",
      x = "Can't request over 99,999 rows of a resource
      AND query its rows/columns.",
      i = "ALL rows and columns of the resource will be downloaded."
    ))
  }

  # warn users if they haven't queried
  # the data but have requested rows > 99999

  if (is.null(rows)) rows <- 0
  if (!queried && rows > 99999) {
    cli::cli_warn(c(
      "Getting all rows of resource.",
      i = "All rows will be returned if you
      request over 99,999 rows of data.",
      i = "You set {.var rows} to
      {format(rows, big.mark = ',', scientific = FALSE)}"
    ))
  }

  return(use_dump)
}
