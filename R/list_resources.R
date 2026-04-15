#' Provides an overview of all resources available on the Open Data platform.
#'
#' @description Provides an overview of all resources available from
#'  [opendata.nhs.scot](https://www.opendata.nhs.scot/), with the option to limit results based on both package
#'  and resource names. The returned tibble can be used to look-up dataset
#'  and resource ids, and is useful for exploring the available data sets.
#'
#' @param dataset_contains A character string containing an expression to be
#'  used as search criteria against the dataset 'title' field.
#' @param resource_contains A character string containing a [regular expression](https://www.geeksforgeeks.org/dsa/write-regular-expressions/)
#'  to be matched against available resource names.
#' @param dataset_name Deprecated. Use dataset_contains instead.
#'
#' @return A [tibble][tibble::tibble-package] containing details of all available datasets and
#'  resources, or those containing the string specified in the
#'  `dataset_contains` and `resource_contains` arguments.
#'
#' @examples
#'
#' list_resources()
#' list_resources(dataset_contains = "standard-populations")
#' list_resources(
#'   dataset_contains = "standard-populations", resource_contains = "European"
#' )
#'
#' @export
list_resources <- function(
  dataset_contains = NULL,
  resource_contains = NULL,
  dataset_name = lifecycle::deprecated()
) {
  # Handling any use of "list_resources(dataset_name)"
  if (lifecycle::is_present(dataset_name)) {
    lifecycle::deprecate_warn(
      when = "1.0.3",
      what = "list_resources(dataset_name)",
      with = "list_resources(dataset_contains)"
    )
    dataset_contains <- dataset_name
  }

  # Normalise empty/whitespace strings to NULL
  normalise <- function(string) {
    if (is.null(string)) {
      return(NULL)
    }
    if (!is.character(string)) {
      return(string)
    }
    if (length(string) != 1L) {
      return(string)
    }
    if (is.na(string)) {
      return(NULL)
    }
    string_trimmed <- trimws(string)
    if (identical(string_trimmed, "")) {
      return(NULL)
    }
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

  validate_regex <- function(pattern, arg_name, call = rlang::caller_env()) {
    if (is.null(pattern)) {
      return(invisible(TRUE))
    }
    tryCatch(
      {
        suppressWarnings(grepl(pattern, "", perl = TRUE))
        TRUE
      },
      error = function(e) {
        cli::cli_abort(
          c(
            "i" = "{.arg {arg_name}} must be a valid regular expression.",
            "x" = e$message
          ),
          call = call
        )
      }
    )
  }

  validate_regex(resource_contains, "resource_contains")
  validate_regex(dataset_contains, "dataset_contains")

  data_tibble <- list_resources_query()

  build_and_anywhere_regex <- function(query) {
    # Split on one-or-more whitespace
    tokens <- strsplit(query, "\\s+")[[1]]
    tokens <- tokens[nzchar(tokens)]

    # Ensures all elements in 'tokens' appear somewhere in the string
    paste0("(?=.*", tokens, ")", collapse = "")
  }

  if (!is.null(resource_contains)) {
    resource_contains_regex <- build_and_anywhere_regex(resource_contains)

    data_tibble <- data_tibble[
      grepl(
        resource_contains_regex,
        data_tibble$resource_name,
        ignore.case = TRUE,
        perl = TRUE
      ),
    ]
    if (nrow(data_tibble) == 0) {
      cli::cli_warn(
        "No resources found for {.arg resource_contains} = {.val {resource_contains}}. Returning an empty tibble."
      )
    }
  }

  if (!is.null(dataset_contains)) {
    dataset_contains_regex <- build_and_anywhere_regex(dataset_contains)

    name_match <- grepl(
      dataset_contains_regex,
      data_tibble$dataset_name,
      ignore.case = TRUE,
      perl = TRUE
    )

    title_match <- grepl(
      dataset_contains_regex,
      data_tibble$dataset_title,
      ignore.case = TRUE,
      perl = TRUE
    )

    data_tibble <- data_tibble[name_match | title_match, ]

    if (nrow(data_tibble) == 0) {
      cli::cli_warn(
        "No resources found for the provided arguments. Returning an empty tibble."
      )
    }
  }

  return(data_tibble)
}


list_resources_query <- function() {
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
        dataset_title = purrr::chuck(dataset, "title"),
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
