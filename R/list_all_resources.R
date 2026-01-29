#' Provides an overview of all resources available on the Open Data platform.
#'
#' @description Provides an overview of all resources available from
#'  [opendata.nhs.scot](https://www.opendata.nhs.scot/), with the option to limit results based on both package
#'  and resource names. The returned tibble can be used to look-up dataset
#'  and resource ids and is useful for exploring the available data sets.
#'
#' @param dataset_contains  a character string containing an expression to be
#'  used as search criteria against the dataset 'title' field.
#' @param resource_contains a character string containing a [regular expression](https://www.geeksforgeeks.org/dsa/write-regular-expressions/)
#'  to be matched against available resource names.
#'
#' @return A [tibble][tibble::tibble-package] containing details of all available datasets and
#'  resources, or those containing the string specified in the
#'  `dataset_contains` and `resource_contains` arguments.
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
list_all_resources <- function(
  dataset_contains = NULL,
  resource_contains = NULL
) {
  # Normalise empty/whitespace strings to NULL
  normalise <- function(string) {
    if (is.null(string)) return(NULL)
    if (!is.character(string)) return(string)
    if (length(string) != 1L) return(string)
    if (is.na(string)) return(NULL)
    string_trimmed <- trimws(string)
    if (identical(string_trimmed, "")) return(NULL)
    return(string_trimmed)
  }
  dataset_contains <- normalise(dataset_contains)
  resource_contains <- normalise(resource_contains)

  # Validate that `dataset_contains` is NULL or a length-1 value
  if (!is.null(dataset_contains)) {
    if (!inherits(dataset_contains, "character")) {
      cli::cli_abort(c(
        "i" = "{.arg dataset_contains} must be {.class character} not a {.class {class(dataset_contains)}}."
      ))
    } else if (length(dataset_contains) != 1) {
      cli::cli_abort(c(
        "x" = "{.arg dataset_contains} must be {.val NULL} or length 1 not length {length(dataset_contains)}.",
        "i" = "Provide a single string (or leave it NULL) for this filter."
      ))
    }
  }

  # Validate that `resource_contains` is NULL or a length-1 value
  if (!is.null(resource_contains)) {
    if (!inherits(resource_contains, "character")) {
      cli::cli_abort(c(
        "i" = "{.arg resource_contains} must be {.class character} not a {.class {class(resource_contains)}}."
      ))
    } else if (length(resource_contains) != 1) {
      cli::cli_abort(c(
        "!" = "{.arg resource_contains} must be {.val NULL} or length 1 not length {length(resource_contains)}.",
        "i" = "Provide a single string (or leave it NULL) for this filter."
      ))
    }
  }

  data_tibble <- list_all_resources_query()


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
