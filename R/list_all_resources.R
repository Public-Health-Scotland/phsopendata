#' Provides an overview of all resources available from <opendata.nhs.scot>.
#'
#' @description Provides an overview of all resources available from
#'  <opendata.nhs.scot>, with the option to limit results based on both package
#'  and resource names. The returned data.frame can be used to look-up package
#'  and resource ids and is useful for exploring the available data sets.
#'
#' @param dataset_contains  a character string containing an expression to be
#'  used as search criteria against the dataset 'title' field.
#' @param resource_contains a character string containing a regular expression
#'  to be matched against available resource names. If a character vector >
#'  length 1 is supplied, the first element is used.
#'
#' @return A [tibble][tibble::tibble-package] containing details of all available datasets and
#'  resources, or those containing the string specified in the
#'  \code{dataset_contains} and \code{resource_contains} arguments.
#'
#' @examples
#'
#' list_all_resources()
#' list_all_resources(dataset_contains = "standard-populations")
#' list_all_resources(
#'   dataset_contains = "standard-populations", resource_contains = "European"
#' )
#'
#' @export

list_all_resources <- function(dataset_contains = NULL, resource_contains = NULL) {
  # Validate that `dataset_contains` is NULL or a length-1 value
  if (!is.null(dataset_contains) && length(dataset_contains) != 1) {
    cli::cli_abort(c(
      "!" = "`.dataset_contains` must be {.emph NULL} or a {.emph length-1} value.",
      "x" = "Current length: {length(dataset_contains)}",
      "i" = "Provide a single string (or leave it NULL) for this filter."
    ))
  }

  # Validate that `resource_contains` is NULL or a length-1 value
  if (!is.null(resource_contains) && length(resource_contains) != 1) {
    cli::cli_abort(c(
      "!" = "`.resource_contains` must be {.emph NULL} or a {.emph length-1} value.",
      "x" = "Current length: {length(resource_contains)}",
      "i" = "Provide a single string (or leave it NULL) for this filter."
    ))
  }


  # query for the API call
  query <- "q=*:*&rows=32000"

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
    resource_name = resources_df$name,
    resource_id = resources_df$id,
    dataset_name = pkgs[unlist(resources_df$package_id)],
    dataset_id = resources_df$package_id,
    url = resources_df$url,
    last_modified = resources_df$last_modified
  )


  if (!is.null(resource_contains)) {
    data_tibble <- data_tibble[grepl(as.character(resource_contains), data_tibble$resource_name, ignore.case = TRUE), ]
    if (nrow(data_tibble) == 0) {
      cli::cli_warn(
        "No resources found for arguments provided. Returning empty data.frame."
      )
    }
  }

  if (!is.null(dataset_contains)) {
    data_tibble <- data_tibble[grepl(as.character(dataset_contains), data_tibble$dataset_name, ignore.case = TRUE), ]
    if (nrow(data_tibble) == 0) {
      cli::cli_warn(
        "No packages found for arguments provided. Returning empty data.frame."
      )
    }
  }

  return(data_tibble)
}
