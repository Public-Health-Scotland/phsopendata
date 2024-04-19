add_context <- function(data, id, name, created_date, modified_date) {
  # Catch if the resource has never been modified
  if (is.null(modified_date)) {
    modified_date <- NA_character_
  }

  # Parse the date values
  created_date <- strptime(created_date, format = "%FT%X", tz = "UTC")
  modified_date <- strptime(modified_date, format = "%FT%X", tz = "UTC")

  data_with_context <- dplyr::mutate(
    data,
    "res_id" = id,
    "res_name" = name,
    "res_created_date" = created_date,
    "res_modified_date" = modified_date,
    .before = dplyr::everything()
  )

  return(data_with_context)
}
