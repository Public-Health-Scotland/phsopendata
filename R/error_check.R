#' Throws errors found in the content of an `httr::GET` request
#'
#' @param content object produced by `httr::content`
#' @keywords internal
#' @noRd
error_check <- function(content, call = rlang::caller_env()) {
  # if content is not a list,
  # stop for content (a string describing an error)
  if (!is.list(content)) {
    cli::cli_abort(
      c(
        "API error",
        x = content
      ),
      call = call
    )
  }

  # if there is no error status/message in the content,
  # break out of the function
  is_error <- suppressWarnings(
    !is.null(content$error)
  )
  if (!is_error) {
    return()
  }

  # generate error message and stop
  error_text <- parse_error(content$error)
  cli::cli_abort(
    c(
      "API error.",
      x = error_text
    ),
    call = call
  )
}
