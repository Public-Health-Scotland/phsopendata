#' Create fields query for GET
#' @description Produces a comma separated list
#'
#' @param col_select a character vector identifying the columns to select.
#' @return a character string
#' @keywords internal
#' @noRd
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
