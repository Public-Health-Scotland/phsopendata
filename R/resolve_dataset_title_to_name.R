#' Resolve a dataset title to its dataset name
#'
#' @description
#' Lets a user pass a human-readable dataset title (e.g. "Dispenser Location
#' Contact Details") where [get_dataset()] expects the dataset name (e.g.
#' "dispenser-location-contact-details"). Inputs that already look like a name,
#' or that are otherwise not a single string, are passed through to the
#' [check_dataset_name()] validator.
#'
#' @details
#' Matching is exact and case-insensitive against the titles of every dataset on
#' the platform, retrieved via a single `package_search` call. This assumes the
#' platform holds fewer than 10,000 datasets. A unique match returns the name
#' with a warning that titles are not a stable key. An ambiguous match, or
#' absence of an exact match, leads to an error that lists candidate titles to
#' help the user correct their input.
#'
#' @param dataset_name A single string: either a dataset name or a dataset title.
#'
#' @return A single character string: the resolved dataset name when the input
#' is a title matching exactly one dataset, or the input unchanged when it looks
#' like a dataset name or is not a single string. A title that matches no
#' dataset, or more than one, raises an error rather than returning.
#'
#' @keywords internal
#' @noRd
resolve_dataset_title_to_name <- function(dataset_name) {
  # return malformed input unchanged for check_dataset_name()
  if (
    !inherits(dataset_name, "character") ||
      length(dataset_name) != 1 ||
      is.na(dataset_name)
  ) {
    return(dataset_name)
  }

  # if input looks like a dataset name, return unchanged for check_dataset_name()
  if (grepl("^[a-z0-9-]+$", dataset_name)) {
    return(dataset_name)
  }

  # extract the results
  datasets <- dplyr::distinct(
    list_resources_query(),
    .data$dataset_name,
    .data$dataset_title
  )

  # check input for an exact match with all_titles (case insensitive)
  exact_index <- tolower(datasets$dataset_title) == tolower(dataset_name)

  # if there is exactly one match, resolve it with a warning
  if (sum(exact_index) == 1) {
    resolved_name <- datasets$dataset_name[exact_index]

    cli::cli_warn(c(
      "Dataset title {.val {dataset_name}} resolved to name {.val {resolved_name}}.",
      "i" = "Dataset titles can change. Use the name for a stable reference."
    ))

    return(resolved_name)
  } else {
    suggest_dataset_name(title = dataset_name)
  }
}
