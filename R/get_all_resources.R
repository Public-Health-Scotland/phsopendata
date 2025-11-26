#' Provides an overview of all resources available from <opendata.nhs.scot>.
#'
#' @description Provides an overview of all resources available from
#'  <opendata.nhs.scot>, with the option to limit results based on both package
#'  and resource names. The returned data.frame can be used to look-up package
#'  and resource ids and is useful for exploring the available data sets.
#'
#' @param package_contains  a character string containing an expression to be
#'  used as search criteria against the packages 'title' field.
#' @param resource_contains a character string containing a regular expression
#'  to be matched against available resource names. If a character vector >
#'  length 1 is supplied, the first element is used.
#'
#' @return A [tibble][tibble::tibble-package] containing details of all available packages and
#'  resources, or those containing the string specified in the
#'  \code{package_contains} and \code{resource_contains} arguments.
#'
#' @examples
#'
#' get_all_resources()
#' get_all_resources(package_contains = "standard-populations")
#' get_all_resources(
#'   package_contains = "standard-populations", resource_contains = "European"
#' )
#'
#'
#' @export

get_all_resources <- function(package_contains = NULL, resource_contains = NULL) {

  stopifnot(is.null(package_contains) || length(package_contains)== 1)
  stopifnot(is.null(resource_contains) || length(resource_contains)== 1)

  #query for the API call
  query <- if (is.null(package_contains)) {
    "q=*:*&rows=32000"
  } else {
    glue::glue("q=title:{package_contains}&rows=32000")
  }
  #API call
  content <- phs_GET(action = "package_search", query = query)
  #extract the data
  extracted_content <- jsonlite::fromJSON(jsonlite::toJSON(content$result))$results

  #Create a named vector of package names keyed by package IDs
  pkgs <- unlist(extracted_content$name)
  names(pkgs) <- unlist(extracted_content$id)

  #extract resources
  resources <- jsonlite::fromJSON(jsonlite::toJSON(extracted_content$resources), flatten = TRUE)
  #Combine all resources into one
  resources_df <- purrr::list_rbind(resources)

  #tidying up
  data_tibble <- tibble::tibble(
    resource_name = resources_df$name,
    resource_id = resources_df$id,
    package_name = pkgs[unlist(resources_df$package_id)],
    package_id = resources_df$package_id,
    url = resources_df$url,
    last_modified = resources_df$last_modified
  )


  if (!is.null(resource_contains)) {
    data_tibble <- data_tibble[grepl(as.character(resource_contains), data_tibble$resource_name, ignore.case = TRUE),]
    if (nrow(out) == 0)  {
      cli::cli_warn(
        "No resources found for arguments provided. Returning empty data.frame."
      )
    }
  }

  return(data_tibble)
}

