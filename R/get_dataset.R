#' Get Open Data resources from a dataset
#'
#' @description Downloads multiple resources from a dataset on the NHS Open Data
#'  platform by dataset name, with optional row limits and context columns.
#'
#' @param dataset_name Name of the dataset as found on the
#' [NHS Open Data platform](https://www.opendata.nhs.scot) (character).
#'
#' @param max_resources (optional) The maximum number of resources to return
#' (integer). If not set, all resources are returned.
#' @inheritParams get_resource
#'
#' @details
#' A human-readable dataset title may also be submitted as `dataset_name`.
#' This is resolved to the corresponding name with a warning, because titles
#' can change while names are stable. Note that title resolution is a feature
#' of `get_dataset()` only and other functions taking `dataset_name` as an
#' argument still require the name itself.
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
  max_resources = NULL,
  rows = NULL,
  row_filters = NULL,
  col_select = NULL,
  include_context = FALSE
) {
  # resolve a title to the dataset name; submitted names pass through
  dataset_name <- resolve_dataset_title_to_name(dataset_name)

  # throw error if name type/format is invalid
  check_dataset_name(dataset_name)

  datasets <- list_resources_query()

  if (!dataset_name %in% datasets$dataset_name) {
    suggest_dataset_name(name = dataset_name)
  }

  dataset_info <- datasets[datasets$dataset_name == dataset_name, ]

  # define list of resource IDs to get
  all_ids <- dataset_info$resource_id

  n_res <- length(all_ids)
  res_index <- seq_len(min(n_res, max_resources))

  selection_ids <- all_ids[res_index]

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
        name = dataset_info[res_index, ]$resource_name,
        created_date = dataset_info[res_index, ]$created,
        modified_date = dataset_info[res_index, ]$last_modified
      ),
      add_context
    )
  }

  # Combine the list of resources into a single tibble
  purrr::list_rbind(all_data)
}
