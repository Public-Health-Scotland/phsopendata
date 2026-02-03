list_all_resources_query <- function() {
  # query for the API call
  query <- list(q = "*:*", rows = 3200)

  # API call
  content <- phs_GET(action = "package_search", query = query)
  # extract the data
  extracted_content <- jsonlite::fromJSON(jsonlite::toJSON(content$result))$results

  # Create a named vector of package names keyed by package IDs
  pkgs <- unlist(extracted_content$name)
  names(pkgs) <- unlist(extracted_content$id)

  # extract resources
  resources <- jsonlite::fromJSON(jsonlite::toJSON(extracted_content$resources), flatten = TRUE)
  # Combine all resources into one
  resources_df <- purrr::list_rbind(resources)

  # tidying up
  data_tibble <- tibble::tibble(
    resource_name = unlist(resources_df$name),
    resource_id = unlist(resources_df$id),
    dataset_name = pkgs[unlist(resources_df$package_id)],
    dataset_id = unlist(resources_df$package_id),
    url = unlist(resources_df$url),
    last_modified = unlist(resources_df$last_modified)
  )

  return(data_tibble)
}
