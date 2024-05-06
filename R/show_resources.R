show_resources <- function(dataset_name){
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

  # define list of resource IDs and names within dataset
  all_ids <- purrr::map_chr(content$result$resources, ~ .x$id)
  all_names <- purrr::map_chr(content$result$resources, ~ .x$name)
  return_value <- list("id" = all_ids, "names" = all_names)

  return(return_value)
  }
