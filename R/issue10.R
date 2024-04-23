
dataset_name <- "community-pharmacy-contractor-activity"

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

# define list of resource IDs to get
all_ids <- purrr::map_chr(content$result$resources, ~ .x$id)
all_names <- purrr::map_chr(content$result$resources, ~ .x$name)
return_value <- tibble("id" = all_ids, "names" = all_names)
