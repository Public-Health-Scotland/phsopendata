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
resolve_dataset_name <- function(dataset_name) {
  # return malformed input unchanged for check_dataset_name()
  if (!inherits(dataset_name, "character") ||
    length(dataset_name) != 1 ||
    is.na(dataset_name)) {
    return(dataset_name)
  }

  # if input looks like a dataset name, return unchanged for check_dataset_name()
  if (grepl("^[a-z0-9-]+$", dataset_name)) {
    return(dataset_name)
  }

  # otherwise, fetch every dataset title and name for matching with input
  # capped at 10000
  content <- phs_GET(
    "package_search",
    list(
      "q" = "*:*",
      "rows" = 10000
    )
  )

  # extract the results
  results <- content$result$results

  # extract dataset titles from results; missing data becomes NA
  all_titles <- purrr::map_chr(
    results,
    function(x) {
      if (is.null(x$title)) NA_character_ else x$title
    }
  )

  # extract dataset names from results; missing data becomes NA
  all_names <- purrr::map_chr(
    results,
    function(x) {
      if (is.null(x$name)) NA_character_ else x$name
    }
  )

  # create a logical vector that's TRUE where titles AND names are not NA
  dataset_index <- !is.na(all_titles) & !is.na(all_names)

  # use this vector to drop results where title is not paired with name
  all_titles <- all_titles[dataset_index]
  all_names <- all_names[dataset_index]

  # check input for an exact match with all_titles (case insensitive)
  exact_index <- tolower(all_titles) == tolower(dataset_name)

  # if there is exactly one match, resolve it with a warning
  if (sum(exact_index) == 1) {
    resolved_name <- all_names[exact_index]

    cli::cli_warn(c(
      "Dataset title {.val {dataset_name}} resolved to name {.val {resolved_name}}.",
      "i" = "Dataset titles can change. Use the name for a stable reference."
    ))

    return(resolved_name)
  }

  # if more than one exact title match, abort and show matching titles and names
  if (sum(exact_index) > 1) {
    matches <- paste0(
      all_titles[exact_index],
      " (",
      all_names[exact_index],
      ")"
    )

    cli::cli_abort(c(
      "x" = "Dataset title {.val {dataset_name}} matched more than one dataset.",
      "i" = paste0("Matching candidates: ", paste(matches, collapse = "; "))
    ))
  }

  # search for input dataset_name as substring within all_titles (case insensitive)
  candidate_index <- grepl(
    tolower(dataset_name),
    tolower(all_titles),
    fixed = TRUE
  )

  # create message, handling no-match case
  candidate_msg <-
    if (any(candidate_index)) {
      candidates <- paste0(all_titles[candidate_index], " (", all_names[candidate_index], ")")
      paste0("Candidates: ", paste(candidates, collapse = "; "))
    } else {
      "Candidates: none"
    }

  ## abort with error message
  cli::cli_abort(c(
    "x" = "Dataset title {.val {dataset_name}} did not match any dataset title exactly.",
    "i" = candidate_msg
  ))
}
