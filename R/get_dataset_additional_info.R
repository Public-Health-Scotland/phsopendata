get_datasets_additional_info <- function(dataset_name){
  query <- paste0("id=", dataset_name)
  content <- phs_GET("package_show", query)

  amount_of_resources <- content$result$resources %>%
    length()

  last_resource_created_date <- purrr::map(content$result$resources, ~.$created) %>%
    unlist() %>%
    max()

  last_resource_modified_date <- purrr::map(content$result$resources, ~.$last_modified) %>%
    unlist() %>%
    max()

  most_recent_resource_date <- c(last_resource_modified_date, last_resource_created_date) %>%
    max()

  return_value <- list("name" = dataset_name,
                         "amount_of_resources" = amount_of_resources,
                         "most_recent_resource_date" = most_recent_resource_date)

  return(return_value)

}


