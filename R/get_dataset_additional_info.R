#' returns a list of dataset names alongwith additional information,
#' such as the amount of resources and the date it was last updated
#'
#' @param dataset_name
#'
#' @return
#' @export
get_dataset_additional_info <- function(dataset_name){
  query <- list("id" = dataset_name)
  content <- phs_GET("package_show", query)

  amount_of_resources <- content$result$resources %>%
    length()

  last_resource_created_date <- purrr::map_chr(content$result$resources, ~.$created)

  last_resource_modified_date <- purrr::map_chr(content$result$resources, ~.$last_modified)

  most_recent_resource_date <- max(last_resource_modified_date, last_resource_created_date) %>%
    as.POSIXct(format = "%FT%X", tz = "UTC")

  return_value <- tibble::tibble("name" = dataset_name,
                         "amount_of_resources" = amount_of_resources,
                         "most_recent_resource_update" = most_recent_resource_date)

  return(return_value)

}


