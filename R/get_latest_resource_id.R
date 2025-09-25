#' get_latest_resource_id
#'
#' to be confident that the resource returned is the one intended
#' two conditions have to be met. It has to appear at the top of
#' of the resource list as shown on the open data platform.
#' The order they are returned via the api is the same
#' as they appear on the open data platform. It also
#' has to have the most recent date created
#'
#' There are only some datasets that this functionality
#' is relevant to, these are listed within applicable
#' datasets and are the datasets that keep historic
#' resources instead of over writing them.
#'
#' @inheritParams get_dataset
#'
#' @return a string with the resource id
#' @keywords internal
#' @noRd
get_latest_resource_id <- function(dataset_name, call = rlang::caller_env()) {
  # send the api request
  query <- list("id" = dataset_name)
  content <- phs_GET("package_show", query)

  # retrieve the resource id's from returned contect
  all_ids <- purrr::map_chr(content$result$resources, ~ .x$id)

  # add the id, created date and last_modified to a dataframe
  id <- c()
  created_date <- c()
  modified_date <- c()

  for (res in content$result$resources) {
    id <- append(id, res$id)
    created_date <- append(created_date, res$created)
    modified_date <- append(modified_date, res$last_modified)
  }
  all_id_data <- tibble::tibble(
    id = id,
    created_date = strptime(created_date, format = "%FT%X", tz = "UTC"),
    modified_date = strptime(modified_date, format = "%FT%X", tz = "UTC")
  ) %>%
    dplyr::mutate(most_recent_date_created = max(created_date))

  # get the first row of the resources, this will be the same that appears on the top
  # on the open data platform
  all_id_data_first_row <- all_id_data %>%
    dplyr::slice(1)

  # If the resource at the top as appearing on the open data platform also has the most
  # recent date created, return it. Otherwise, error
  if (
    all_id_data_first_row$created_date ==
      all_id_data_first_row$most_recent_date_created
  ) {
    return(all_id_data_first_row$id)
  }
  cli::cli_abort("The most recent id could not be identified", call = call)
}
