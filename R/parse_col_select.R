#' Create fields query for GET
#'
#' @description Produces a comma separated list of column names for use in a GET request.
#'
#' @param col_select A character vector containing the names of desired columns/fields (e.g., c("Date", "Sex")).
#'
#' @return a character string
#' @noRd
#' @keywords internal
parse_col_select <- function(col_select, call = rlang::caller_env()) {
  if (is.null(col_select)) {
    return(NULL)
  }

  if (inherits(col_select, "list")) {
    col_select <- unlist(col_select)
  }

  # Remove any duplicates
  col_select <- unique(col_select)

  if (!inherits(col_select, "character")) {
    cli::cli_abort(
      "{.arg col_select} must be a {.cls character} vector, not a {.cls {class(col_select)}} vector.",
      call = call
    )
  }

  return(paste0(col_select, collapse = ","))
}
