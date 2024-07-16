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
get_latest_resource_id <- function(dataset_name) {
  applicable_datasets <- c(
    "gp-practice-populations", "gp-practice-contact-details-and-list-sizes",
    "nhsscotland-payments-to-general-practice", "dental-practices-and-patient-registrations",
    "general-practitioner-contact-details", "prescribed-dispensed",
    "prescriptions-in-the-community", "community-pharmacy-contractor-activity"
  )

  # throw error if name type/format is invalid
  check_dataset_name(dataset_name)

  # define query and try API call
  query <- list("id" = dataset_name)
  content <- try(
    phs_GET("package_show", query),
    silent = TRUE
  )


  # check if data set is within applicable datasets
  # throw error if not
  if (!dataset_name %in% applicable_datasets) {
    cli::cli_abort(c(
      "The dataset name supplied {.var {dataset_name}} is not within the applicable datasets.
      These are:\n
      {.var {applicable_datasets}}",
      "x" = "Please see get_latest_reource documentation.",
      "i" = "You can find dataset names in the URL
      of a dataset's page on {.url www.opendata.nhs.scot}."
    ), 
    call = rlang::caller_env())
  }

  # if content contains a 'Not Found Error'
  # throw error with suggested dataset name
  if (grepl("Not Found Error", content[1])) {
    suggest_dataset_name(dataset_name)
  }

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

  # get the first row of the rources, this will be the same that appears on the top
  # on the open data platform
  all_id_data_first_row <- all_id_data %>%
    dplyr::slice(1)

  # if the resource at the top as appearing on the open data platform also has the most
  # recent date created, return it. Otherwise return warning
  if (all_id_data_first_row$created_date == all_id_data_first_row$most_recent_date_created) {
    return(all_id_data_first_row$id)
  } else {
    (warning("most recent id could not be identified"))
  }
}
