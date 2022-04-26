#' Create JSON 'dict' from named list or vector
#' @description Formats a list or named vector into a valid query
#' @param row_filters list or named vectors matching fileds to values
#' @return a json as a character string
parse_row_filters <- function(row_filters) {

  # exit function if no filters
  if (is.null(row_filters))
    return(NULL)

  # check if any filters in list have length > 1
  too_many <- sapply(row_filters, length) > 1

  if (any(too_many))
    cli::cli_abort(c(
      "Invalid input for {.var row_filters}",
      i = "{names(row_filters)[which(too_many)]} in {.var row_filters} has too many values. ",
      x = "The {.var row_filters} list must only contain vectors of length 1."
    ))

  # check if any items in the list/vector have the same name
  # find number of unique names
  n_u_row_filters <- length(unique(names(row_filters)))
  # find total number of names
  n_row_filters <- length(names(row_filters))
  # if same, all names are unique
  unique_names <- n_u_row_filters == n_row_filters

  if (!unique_names)
    cli::cli_abort(c(
      "Invalid input for {.var row_filters}",
      x = "One or more elements in {.var row_filters} have the same name.",
      i = "Only one filter per field is currently supported by `get_resource`."
    ))

  filter_body <- paste0(
    '"', names(row_filters), '":"', row_filters, '"', collapse = ","
  )

  return(
    paste0('{', filter_body, '}')
  )

}
