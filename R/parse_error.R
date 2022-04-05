#' Generates error message from an "error" element of an `httr::content` object
#'
#' @param error "error" element of an object produced by `httr::content`
#' @return character string
#'
parse_error <- function(error) {
  error_message <- error$message
  error_type <- error$`__type`

  # define error text
  error_output <- paste0(
    error_type, ": ", error_message
  )

  # special case for validation errors
  if (error_type == "Validation Error") {

    error_output <- paste0(
      names(error[1][1]), ": ",
      error[1][[1]]
    )

    # translate message for package users
    error_output <- sub(
      "fields",
      "col_select",
      error_output
    )

    error_output <- sub(
      "q",
      "row_filters",
      error_output
    )

  }

  return(error_output)
}
