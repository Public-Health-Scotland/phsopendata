#' get a datasets additional info
#'
#' `get_dataset_additional_info()` returns a tibble of dataset names along with
#' the amount of resources it has and the date it was last updated.Last updated
#' is taken to mean the most recent date a resource within the dataset was
#' created or modified.
#'
#' @inheritParams get_dataset
#'
#' @return a [tibble][tibble::tibble-package] with the data
#' @export
#' @examples
#' get_dataset_additional_info("gp-practice-populations")
get_dataset_additional_info <- function(dataset_name) {
  # define query

  query <- list("id" = dataset_name)
  # fetch the data
  content <- phs_GET("package_show", query)

  # get the amount of resources
  amount_of_resources <- content$result$resources %>%
    length()

  # get the last recourse created and modified dates
  last_resource_created_date <- purrr::map_chr(
    content$result$resources,
    ~ .$created
  )
  last_resource_modified_date <- purrr::map_chr(
    content$result$resources,
    ~ .$last_modified
  )

  # get the latest between the created and modified dates and change to datetime format
  most_recent_resource_date <- max(
    last_resource_modified_date,
    last_resource_created_date
  ) %>%
    as.POSIXct(format = "%FT%X", tz = "UTC")

  # create tibble to return
  return_value <- tibble::tibble(
    "name" = dataset_name,
    "n_resources" = amount_of_resources,
    "last_updated" = most_recent_resource_date
  )

  return(return_value)
}
