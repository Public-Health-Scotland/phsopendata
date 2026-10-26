#' Resolve a resource title to its resource ID
#'
#' @description
#' Lets a user pass a human-readable resource title where [get_resource()]
#' expects a resource ID. Inputs that already look like an ID, or that are
#' otherwise not a single string, are returned for subsequent validation by
#' [check_res_id()] in [get_resource()].
#'
#' @details
#' Matching is exact and case-insensitive against resource titles retrieved
#' via [list_resources_query()]. A unique match returns the ID with a warning
#' that titles are not a stable key. Input and catalogue titles are trimmed of
#' surrounding white space for matching. Ambiguous entries raise an error
#' listing all matches.
#' Inputs with no exact title match raise an error listing up to ten resources
#' whose titles contain any whitespace-separated term from the input as a
#' substring, using fixed-string, case-insensitive matching.
#'
#' @param res_id A single string: either a resource ID or a resource title.
#'
#' @return The resource ID when exactly one resource title matches.
#' ID-shaped inputs and empty strings are returned after trimming surrounding
#' white space. Non-character, non-scalar or missing inputs are returned unchanged
#' for subsequent validation.
#' A title that matches no resource, or more than one, raises an error.
#'
#' @keywords internal
#' @noRd
resolve_resource_title_to_id <- function(res_id) {
  # return invalid input unchanged for check_res_id()
  if (
    !inherits(res_id, "character") ||
    length(res_id) != 1 ||
    is.na(res_id)
  ) {
    return(res_id)
  }
  # trim any white space and return if empty
  res_id <- trimws(res_id)
  if (res_id == "") {
    return(res_id)
  }
  # if input looks like a resource ID, return unchanged for check_res_id()
  if (grepl(
    "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$",
    res_id,
    ignore.case = TRUE # unlike check_res_id(), accepts uppercase letters
  )) {
    return(res_id)
  }
  # extract the results
  resources <- list_resources_query()
  resources <- resources[!is.na(resources$resource_name), , drop = FALSE]

  # check input for an exact match with resource titles (case insensitive)
  exact_index <- tolower(trimws(resources$resource_name)) == tolower(res_id)
  # if there is exactly one match, resolve it with a warning
  if (sum(exact_index) == 1) {
    resolved_id <- resources$resource_id[exact_index]
    cli::cli_warn(c(
      "Resource title {.val {res_id}} resolved to ID {.val {resolved_id}}.",
      "i" = "Resource titles can change. Use the ID for a stable reference."
    ))
    return(resolved_id)
  }
  # format resource titles, dataset titles and IDs as ordinary strings
  format_matches <- function(matches) {
    sprintf(
      '"%s" in dataset %s (%s)',
      matches$resource_name,
      matches$dataset_title,
      matches$resource_id
    )
  }

  # build one CLI bullet per match without inserting titles into format strings
  match_bullets <- function(n) {
    bullets <- sprintf("{match_lines[[%d]]}", seq_len(n))
    names(bullets) <- rep("*", n)
    bullets
  }

  # if there is more than one match, list every matching resource
  if (sum(exact_index) > 1) {
    match_lines <- format_matches(
      resources[exact_index, , drop = FALSE]
    )

    cli::cli_abort(c(
      "Resource title {.val {res_id}} matched more than one resource.",
      match_bullets(length(match_lines)),
      "i" = "Please use a resource ID instead. Use {.fn list_resources} to explore available resources."
    ))
  }

  # find resource titles containing any search term from the input
  words <- strsplit(res_id, "\\s+", perl = TRUE)[[1]]
  words <- unique(words[nzchar(words)])

  possible_index <- rep(FALSE, nrow(resources))

  for (word in words) {
    possible_index <- possible_index | grepl(
      tolower(word),
      tolower(resources$resource_name),
      fixed = TRUE
    )
  }

  possible_matches <- resources[possible_index, , drop = FALSE]

  # list up to ten possible matches without selecting one automatically
  if (nrow(possible_matches) > 0) {
    match_lines <- format_matches(utils::head(possible_matches, 10))
    candidate_bullets <- match_bullets(length(match_lines))

    if (nrow(possible_matches) > 10) {
      remaining <- nrow(possible_matches) - 10
      candidate_bullets <- c(
        candidate_bullets,
        "i" = "and {remaining} more"
      )
    }
  } else {
    candidate_bullets <- c("i" = "none found")
  }

  cli::cli_abort(c(
    "Resource title {.val {res_id}} did not match any resource title exactly.",
    "i" = "Resources whose titles share any word with the input:",
    candidate_bullets,
    "i" = "Please use a resource ID or an exact resource title. Use {.fn list_resources} to explore available resources."
  ))
}
