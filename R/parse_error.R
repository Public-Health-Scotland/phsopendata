#' Generates error message from an "error" element of an `httr::content` object
#'
#' @param error The "error" element of an object produced by `httr::content`.
#' @return character string
#' @noRd
#' @keywords internal
parse_error <- function(error) {
  error_message <- error$message
  error_type <- error$`__type`

  # define error for most common cases
  error_output <- paste0(
    error_type,
    ": ",
    error_message
  )

  # special case for validation errors
  if (error_type == "Validation Error") {
    error_output <- paste0(names(error[1][1]), ": ", error[1][[1]])

    # translate message for package users
    error_output <- sub("fields", "col_select", error_output)
    error_output <- sub("q", "row_filters", error_output)
  }

  # special case for SQL validation errors
  if (!is.null(error$info$orig)) {
    error_output <- sub("\\^", "", error$info$orig[[1]])
    error_output <- sub("LINE 1:", "in SQL:", error_output)
    error_output <- sub("relation", "resource/table", error_output)
  }

  return(error_output)
}
