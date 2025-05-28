#' Throw an error with suggested dataset name, if possible.
#'
#' @inheritParams get_dataset
#' @keywords internal
#' @noRd
suggest_dataset_name <- function(dataset_name, call = rlang::caller_env()) {
  content <- phs_GET("package_list", "")

  dataset_names <- unlist(content$result)

  # calculate string distances
  string_distances <- stringdist::stringdist(dataset_name, dataset_names)

  # if min distance is too big, abort
  if (min(string_distances) > 10) {
    cli::cli_abort(
      c(
        "Can't find the dataset name
      {.var {dataset_name}}, or a close match.",
        i = "Find a dataset's name in the URL
      of its page on {.url www.opendata.nhs.scot.}"
      ),
      call = call
    )
  }

  # find closet match
  closest_match <- dataset_names[which(
    string_distances == min(string_distances)
  )]

  # throw error with suggestion
  cli::cli_abort(
    c(
      "Can't find the dataset name {.var {dataset_name}}.",
      "i" = "Did you mean {?any of }{.val {closest_match}}?"
    ),
    call = call
  )
}
