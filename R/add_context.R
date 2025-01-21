#' @keywords internal
#' @noRd
add_context <- function(data, id, name, created_date, modified_date) {
  # Catch if the resource has never been modified
  if (is.null(modified_date)) {
    modified_date <- NA_character_
  }

  # Parse the date values
  created_date <- as.POSIXct(created_date, format = "%FT%X", tz = "UTC")
  modified_date <- as.POSIXct(modified_date, format = "%FT%X", tz = "UTC")

  # The platform can record the modified date as being before the created date
  # by a few microseconds, this will catch any rounding which ensure
  # created_date is always <= modified_date
  if (!is.na(modified_date) && modified_date < created_date) {
    modified_date <- created_date
  }

  data_with_context <- dplyr::mutate(
    data,
    "ResID" = id,
    "ResName" = name,
    "ResCreatedDate" = created_date,
    "ResModifiedDate" = modified_date,
    .before = dplyr::everything()
  )

  return(data_with_context)
}
