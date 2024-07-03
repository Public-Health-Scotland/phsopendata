#' get a datasets additional info
#'
#' `get_dataset_additional_info()`returns a list of dataset names alongwith additional information,
#' such as the amount of resources and the date it was last updated
#'
#' @inheritParams get_dataset
#'
#' @return a [tibble][tibble::tibble-package] with the data
#' @export
#' @examples
#' get_dataset("gp-practice-populations")
get_dataset_additional_info <- function(dataset_name){
  #define query

  query <- list("id" = dataset_name)
  #fetch the data
  content <- phs_GET("package_show", query)

  #get the amount of resources
  amount_of_resources <- content$result$resources %>%
    length()


  #get the last recourse created and modified dates
  last_resource_created_date <- purrr::map_chr(content$result$resources, ~.$created)
  last_resource_modified_date <- purrr::map_chr(content$result$resources, ~.$last_modified)

  #get the latest between the created and modified dates and change to datetime format
  most_recent_resource_date <- max(last_resource_modified_date, last_resource_created_date) %>%
    as.POSIXct(format = "%FT%X", tz = "UTC")


  #create tibble to return
  return_value <- tibble::tibble("name" = dataset_name,
                         "amount_of_resources" = amount_of_resources,
                         "most_recent_resource_update" = most_recent_resource_date)

  return(return_value)
}
