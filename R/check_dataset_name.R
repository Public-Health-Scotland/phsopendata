#' Throws an error if a dataset name is invalid
#'
#' @description
#' Used to attempt to validate a dataset name before submitting it to the API
#'
#' @param dataset_name a resource ID
#' @keywords internal
#' @noRd
check_dataset_name <- function(dataset_name, call = rlang::caller_env()) {
  # Starts and ends in a lowercase letter or number
  # Has only lowercase alphanum or hyphens inbetween
  dataset_name_regex <- "^[a-z0-9][a-z0-9\\-]+?[a-z0-9]$"

  if (!inherits(dataset_name, "character")) {
    cli::cli_abort(
      c(
        "The dataset name supplied {.var {dataset_name}} is invalid.",
        "x" = "dataset_name must be of type character.",
        "i" = "You supplied a {.cls {class(dataset_name)[0]}} value."
      ),
      call = call
    )
  }

  if (!grepl(dataset_name_regex, dataset_name)) {
    cli::cli_abort(
      c(
        "The dataset name supplied {.var {dataset_name}} is invalid",
        "x" = "dataset_name must be in dash-case
      (e.g., lowercase-words-separated-by-dashes).",
        "i" = "You can find dataset names in the URL
      of a dataset's page on {.url www.opendata.nhs.scot}."
      ),
      call = call
    )
  }
}
