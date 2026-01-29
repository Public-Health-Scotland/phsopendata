list_all_resources_query <- function() {
  # query for the API call
  query <- list(q = "*:*", rows = 3200)

  # API call
  content <- phs_GET(action = "package_search", query = query)

  # extract the data
  results <- purrr::chuck(content, "result", "results")

  datasets_list <- purrr::map(
    results,
    function(dataset) {
      resources <- purrr::chuck(dataset, "resources")

      tibble::tibble(
        resource_name = purrr::map_chr(
          resources,
          ~ purrr::pluck(.x, "name", .default = NA_character_)
        ),
        resource_id = purrr::map_chr(
          resources,
          ~ purrr::pluck(.x, "id", .default = NA_character_)
        ),
        dataset_name = purrr::chuck(dataset, "name"),
        dataset_id = purrr::chuck(dataset, "id"),
        url = purrr::map_chr(
          resources,
          ~ purrr::pluck(.x, "url", .default = NA_character_)
        ),
        last_modified = as.POSIXct(
          purrr::map_chr(
            resources,
            ~ purrr::pluck(.x, "last_modified", .default = NA_character_)
          ),
          format = "%FT%X",
          tz = "UTC"
        )
      )
    }
  )

  data_tibble <- purrr::list_rbind(datasets_list)

  return(data_tibble)
}
