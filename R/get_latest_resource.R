get_latest_resource <- function(dataset_name, max_resources = NULL, rows = NULL){
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

  query <- list("id" = dataset_name)
  content <- try(
    phs_GET("package_show", query),
    silent = TRUE
  )

  all_ids <- purrr::map_chr(content$result$resources, ~ .x$id)
  res_index <- 1:length(all_ids)

  id <- c()
  created_date <- c()
  modified_date <- c()

  for(i in content$result$resources){
    id <-  append(id, i$id)
    created_date <- append(created_date, i$created)
    modified_date <- append(modified_date, i$last_modified)
  }


  all_id_data <- list(id = id,
                      created_date = strptime(created_date, format = "%FT%X", tz = "UTC"),
                      modified_date = strptime(modified_date, format = "%FT%X", tz = "UTC"))

  print(all_id_data)

  #all_id_data

  #df <- data.frame(all_id_data) %>%
  #  subset(created_date == max(created_date))
#
  #df$id
#
}







