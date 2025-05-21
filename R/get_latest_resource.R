#' Get the latest resource from a data set
#'
#' Returns the latest resource available in a dataset.
#'
#' There are some datasets on the open data platform that
#' keep historic resources instead of updating existing ones.
#' For these it is useful to be able to retrieve the latest
#' resource. As of 1.8.2024 these data sets include:
#' * gp-practice-populations
#' * gp-practice-contact-details-and-list-sizes
#' * nhsscotland-payments-to-general-practice
#' * dental-practices-and-patient-registrations
#' * general-practitioner-contact-details
#' * prescribed-dispensed
#' * dispenser-location-contact-details
#' * community-pharmacy-contractor-activity
#'
#' @inheritParams get_dataset
#' @inheritParams get_resource
#'
#' @return a [tibble][tibble::tibble-package] with the data
#' @export
#'
#' @examples
#' dataset_name <- "gp-practice-contact-details-and-list-sizes"
#'
#' data <- get_latest_resource(dataset_name)
#'
#' filters <- list("Postcode" = "DD11 1ES")
#' wanted_cols <- c("PracticeCode", "Postcode", "Dispensing")
#'
#' filtered_data <- get_latest_resource(
#'   dataset_name = dataset_name,
#'   row_filters = filters,
#'   col_select = wanted_cols
#' )
#'
get_latest_resource <- function(
  dataset_name,
  rows = NULL,
  row_filters = NULL,
  col_select = NULL,
  include_context = TRUE
) {
  applicable_datasets <- c(
    "community-pharmacy-contractor-activity",
    "dental-practices-and-patient-registrations",
    "dispenser-location-contact-details",
    "general-practitioner-contact-details",
    "gp-practice-contact-details-and-list-sizes",
    "gp-practice-populations",
    "nhsscotland-payments-to-general-practice",
    "prescribed-dispensed"
  )

  # check if data set is within applicable datasets
  # throw error if not
  if (!dataset_name %in% applicable_datasets) {
    cli::cli_abort(
      c(
        "The dataset name supplied {.val {dataset_name}} is not within the applicable datasets.
      These are: {.val {applicable_datasets}}",
        "x" = "Please see {.fun get_latest_resource} documentation.",
        "i" = "You can find dataset names in the URL
      of a dataset's page on {.url www.opendata.nhs.scot}."
      ),
      call = rlang::caller_env()
    )
  }

  # get the latest resource id
  id <- get_latest_resource_id(dataset_name)

  data <- get_resource(
    res_id = id,
    rows = rows,
    row_filters = row_filters,
    col_select = col_select,
    include_context = include_context
  )

  return(data)
}
