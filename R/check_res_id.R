#' Check if a resource ID is valid
#'
#' @description
#' Used to attempt to validate a res_id before submitting it to the API
#'
#' @inheritParams get_resource
#'
#' @return TRUE / FALSE indicating the validity of the res_id
#' @keywords internal
#' @noRd
check_res_id <- function(res_id, call = rlang::caller_env()) {
  # check res_id is single value
  if (length(res_id) > 1) {
    cli::cli_abort(
      c(
        "Argument {.var res_id} must be of length 1.",
        i = "You supplied a res_id with a length of {length(res_id)}",
        x = "`get_resource` does not currently support
      requests for multiple resources simultaneously."
      ),
      call = call
    )
  }

  # check res_id is character
  if (!inherits(res_id, "character")) {
    cli::cli_abort(
      c(
        "Argument {.var res_id} must be of type character",
        i = "You supplied a {.var res_id} with type  {.cls {class(res_id)[1]}}"
      ),
      call = call
    )
  }

  # check regex pattern
  res_id_regex <-
    "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
  if (!grepl(res_id_regex, res_id)) {
    cli::cli_abort(
      c(
        "Argument {.var res_id} is in an invalid format.",
        i = "You can find a resource's ID in the URL of it's page on {.url www.opendata.nhs.scot}."
      ),
      call = call
    )
  }
}
