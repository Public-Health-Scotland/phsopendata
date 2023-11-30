#' Get Open Data resources from a dataset
#'
#' @param dataset_name name of the dataset as found on
#' \href{https://www.opendata.nhs.scot/}{NHS Open Data platform}
#' @param max_resources (optional) the maximum number of resources
#' to return, use for testing code,
#' it will retunr the n latest resources
#' @param rows (optional) specify the max number of rows
#' to return for each resource.
#'
#' @seealso [get_resource()] for downloading a single resource
#' from a dataset.
#'
#' @importFrom magrittr %>%
#' @return a [tibble][tibble::tibble-package] with the data
#' @export
#'
#' @examples get_dataset("gp-practice-populations",
#'   max_resources = 2, rows = 10
#' )
get_dataset <- function(dataset_name, max_resources = NULL, rows = NULL) {
  # throw error if name type/format is invalid
  check_dataset_name(dataset_name)

  # define query and try API call
  query <- paste0("id=", dataset_name)
  content <- try(
    phs_GET("package_show", query),
    silent = TRUE
  )

  # if content contains a 'Not Found Error'
  # throw error with suggested dataset name
  if (grepl("Not Found Error", content[1])) {
    suggest_dataset_name(dataset_name)
  }

  # define list of resource IDs to get
  all_ids <- purrr::map_chr(content$result$resources, ~ .x$id)
  n_res <- length(all_ids)
  res_index <- 1:min(n_res, max_resources)
  ids_selection <- all_ids[res_index]

  # get all resources
  all_data <- purrr::map(
    ids_selection,
    get_resource,
    rows = rows
  )

  # resolve class issues
  types <- purrr::map(
    all_data,
    ~ unlist(lapply(.x, class))
  )

  # for each df, check if next df class matches
  inconsistencies <- vector(length = length(types) - 1, mode = "list")
  for (i in seq_along(types)) {
    if (i == length(types)) break

    this_types <- types[[i]]
    next_types <- types[[i + 1]]

    # find matching names
    matching_names <- suppressWarnings(
      names(this_types) == names(next_types)
    )

    # of matching name cols, find if types match too
    inconsistent_index <- this_types[matching_names] != next_types[matching_names]
    inconsistencies[[i]] <- this_types[matching_names][inconsistent_index]
  }

  # define which columns to coerce and warn
  conflicts <- unlist(inconsistencies)
  to_coerce <- unique(names(conflicts))

  if (length(to_coerce) > 0) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))
  }

  # combine
  combined <- purrr::map_df(
    all_data,
    ~ dplyr::mutate(
      .x,
      dplyr::across(
        dplyr::any_of(to_coerce),
        as.character
      )
    )
  )

  return(combined)
}
