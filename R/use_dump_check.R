#' Determines if the datastore dump should be used
#'
#' @param query a list of items to query
#' @param rows the number of rows user is requesting
#' @return a logical value. TRUE indicates that the dump should be used
#'
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
  # if user queried the data (excluding rows)
  queried <- !is.null(query$q) || !is.null(query$filter) || !is.null(query$fields)

  if (queried && use_dump)
    cli::cli_warn(c(
      "Invalid combination of {.var rows}, {.var row_filters} and/or {.var col_select}.",
      x = "Can't request over 99,999 rows of a resource AND query its rows/columns.",
      i = "ALL rows and columns of the resource will be downloaded."
    ))

  return(use_dump)
}
