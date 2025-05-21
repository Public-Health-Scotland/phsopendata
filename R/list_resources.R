#' Lists all available resources for a dataset
#'
#' `list_resources()` returns all of the resources associated
#' with a dataset
#'
#' @inheritParams get_dataset
#'
#' @return a [tibble][tibble::tibble-package] with the data
#' @export
#'
#' @examples
#' list_resources("weekly-accident-and-emergency-activity-and-waiting-times")
list_resources <- function(dataset_name) {
  # throw error if name type/format is invalid
  check_dataset_name(dataset_name)

  # define query and try API call
  query <- list("id" = dataset_name)
  content <- try(
    phs_GET("package_show", query),
    silent = TRUE
  )

  # if content contains a 'Not Found Error'
  # throw error with suggested dataset name
  if (grepl("Not Found Error", content[1])) {
    suggest_dataset_name(dataset_name)
  }

  # define list of resource IDs names date created and date modified within dataset
  all_ids <- purrr::map_chr(content$result$resources, ~ .x$id)
  all_names <- purrr::map_chr(content$result$resources, ~ .x$name)
  all_date_created <- purrr::map_chr(content$result$resources, ~ .x$created) %>%
    as.POSIXct(format = "%FT%X", tz = "UTC")
  all_date_modified <- purrr::map_chr(
    content$result$resources,
    ~ .x$last_modified
  ) %>%
    as.POSIXct(format = "%FT%X", tz = "UTC")
  return_value <- tibble::tibble(
    "res_id" = all_ids,
    "name" = all_names,
    "created" = all_date_created,
    "last_modified" = all_date_modified
  )

  return(return_value)
}
