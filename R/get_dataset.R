#' Get Open Data resources from a dataset
#'
#' @description Downloads multiple resources from a dataset on the NHS Open Data
#'  platform by dataset name, with optional row limits and context columns.
#'
#' @param dataset_name Name of the dataset as found on the
#' [NHS Open Data platform](https://www.opendata.nhs.scot) (character).
#' @param max_resources (optional) The maximum number of resources to return
#' (integer). If not set, all resources are returned.
#' @inheritParams get_resource
#'
#' @seealso [get_resource()] for downloading a single resource from a dataset.
#'
#' @return A [tibble][tibble::tibble-package] with the data.
#' @export
#'
#' @examplesIf isTRUE(length(curl::nslookup("www.opendata.nhs.scot", error = FALSE)) > 0L)
#' \dontrun{
#' get_dataset("gp-practice-populations", max_resources = 2, rows = 10)
#' }
get_dataset <- function(
    dataset_name,
    resource_contains,
    max_resources = NULL,
    confirm_latest = FALSE,
    rows = NULL,
    row_filters = NULL,
    col_select = NULL,
    include_context = FALSE
) {
  # throw error if name type/format is invalid
  check_dataset_name(dataset_name)

  # define query and try API call
  query <- list(id = dataset_name)
  content <- try(
    phs_GET("package_show", query),
    silent = TRUE
  )

  # if content contains a 'Not Found Error'
  # throw error with suggested dataset name
  if (grepl("Not Found Error", content[1L], fixed = TRUE)) {
    suggest_dataset_name(dataset_name)
  }

  # define list of resource IDs to get
  if (is.null(resource_contains) & is.null(confirm_latest)) {
    all_ids <- purrr::map_chr(content$result$resources, ~ .x$id)
  }

  if (!is.null(resource_contains) | !is.null(confirm_latest)) {
     all_id_data <- list_resources(dataset_contains = dataset_name, resource_contains = resource_contains)
     all_ids <- all_id_data$resource_id
  }

  n_res <- length(all_ids)
  n_res_selected <- min(n_res, max_resources)
  res_index <- 1L:min(n_res, max_resources)

  selection_ids <- all_ids[res_index]

  # confirm selected ids are latest resources
  if (confirm_latest) {

    # find most recent date_created dates
     most_recent_date_created <- dplyr::slice_max(all_id_data, created_date, n = n_res_selected)

     # get the first n rows of the resources, this will be the same that appears
     # on the top on the open data platform
     all_id_data_first_row <- dplyr::slice_head(all_id_data, n = n_res_selected)

     # If the n resources at the top as appearing on the open data platform match the most
     # recent date created, return it. Otherwise, error
     if (!identical(all_id_data_first_row, most_recent_date_created)
     ) {
       cli::cli_abort("The most recent resource ids could not be identified")
     }
  }

  # get all resources
  all_data <- purrr::map(
    selection_ids,
    get_resource,
    rows = rows,
    row_filters = row_filters,
    col_select = col_select
  )

  # resolve class issues
  types <- purrr::map(
    all_data,
    purrr::map_chr,
    class
  )

  # for each df, check if next df class matches
  inconsistencies <- vector(length = length(types) - 1L, mode = "list")
  for (i in seq_along(types)) {
    if (i == length(types)) break

    this_types <- types[[i]]
    next_types <- types[[i + 1L]]

    # find matching names
    matching_names <- suppressWarnings(
      names(this_types) == names(next_types)
    )

    # of matching name cols, find if types match too
    inconsistent_index <- this_types[matching_names] !=
      next_types[matching_names]
    inconsistencies[[i]] <- this_types[matching_names][inconsistent_index]
  }

  # define which columns to coerce and warn
  to_coerce <- unique(names(unlist(inconsistencies)))

  if (length(to_coerce) > 0L) {
    cli::cli_warn(c(
      "Due to conflicts between column types across resources,
      the following {cli::qty(to_coerce)} column{?s} ha{?s/ve} been coerced to type character:",
      "{.val {to_coerce}}"
    ))

    all_data <- purrr::map(
      all_data,
      dplyr::mutate,
      dplyr::across(
        dplyr::any_of(to_coerce),
        as.character
      )
    )
  }

  if (include_context) {
    # Add the 'resource context' as columns to the data
    all_data <- purrr::pmap(
      list(
        data = all_data,
        id = selection_ids,
        name = purrr::map_chr(content$result$resources[res_index], ~ .x$name),
        created_date = purrr::map_chr(
          content$result$resources[res_index],
          ~ .x$created
        ),
        modified_date = purrr::map_chr(
          content$result$resources[res_index],
          ~ .x$last_modified
        )
      ),
      add_context
    )
  }

  # Combine the list of resources into a single tibble
  combined <- purrr::list_rbind(all_data)

  return(combined)
}
