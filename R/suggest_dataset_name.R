#' Suggest a dataset title from a partial name or title
#' @param name Character string of the dataset name
#' @param title Character string of the dataset title
#' @keywords internal
#' @noRd
suggest_dataset_name <- function(
  name = NULL,
  title = NULL,
  call = rlang::caller_env()
) {
  if (is.null(name) == is.null(title)) {
    cli::cli_abort(
      "You must provide exactly one argument: either {.arg name} or {.arg title}."
    )
  }

  datasets <- dplyr::distinct(
    list_resources_query(),
    .data$dataset_name,
    .data$dataset_title
  )

  looking_for_name <- !is.null(name)
  looking_for_text <- if (looking_for_name) "name" else "title"
  search_term <- if (looking_for_name) name else title
  search_target <- if (looking_for_name) {
    datasets$dataset_name
  } else {
    datasets$dataset_title
  }

  # calculate string distances
  string_distances <- stringdist::stringdist(search_term, search_target)
  min_dist <- min(string_distances)

  # if min distance is too big, abort
  if (min_dist > 10.0) {
    cli::cli_abort(
      c(
        "Can't find the dataset {looking_for_text}
      {.val {search_term}}, or a close match.",
        i = "Find a dataset's name (or title) in the URL
      of its page on {.url www.opendata.nhs.scot.}"
      ),
      call = call
    )
  }

  close_matches <- which(string_distances == min_dist)

  suggestions <- if (looking_for_name) {
    datasets$dataset_name[close_matches]
  } else {
    paste0(
      datasets$dataset_name[close_matches],
      " (title: ",
      datasets$dataset_title[close_matches],
      ")"
    )
  }

  # throw error with suggestion
  cli::cli_abort(
    c(
      "Can't find the dataset {looking_for_text} {.var {search_term}}.",
      i = "Did you mean {?any of }{.val {suggestions}}?"
    ),
    call = call
  )
}
