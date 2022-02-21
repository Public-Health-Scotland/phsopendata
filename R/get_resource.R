#' Get Open Data resource
#'
#' @param res_id The resource ID as found on
#' \href{https://www.opendata.nhs.scot/}{NHS Open Data platform}
#' @param rows (optional) specify the max number of rows to return.
#' @param row_filters (optional) a named list or vector that specifies values of columns/fields to keep.
#' e.g. list(Date = 20220216, Sex = "Female").
#' @param col_select (optional) a character vector containing the names of desired columns/fields.
#' e.g. c("Date", "Sex").
#'
#' @seealso [get_dataset()] for downloading all resources
#' from a given dataset.
#'
#' @importFrom magrittr %>%
#' @return a [tibble][tibble::tibble-package] with the data
#' @export
#'
#' @examples
#' res_id <- "ca3f8e44-9a84-43d6-819c-a880b23bd278"
#' filters <- list("HB" = "S08000030", "Month" = "202109")
#' wanted_cols <- c("HB", "Month", "TotalPatientsSeen")
#'
#' df <- get_resource(res_id = res_id, row_filters = filters, col_select = wanted_cols)
get_resource <- function(res_id, rows = NULL, row_filters = NULL, col_select = NULL) {

  check_connection()
  check_res_id(res_id)

  # Define the User Agent to be used for the API call
  ua <- opendata_ua()

  # define query
  query <- list(
    id = res_id,
    limit = rows,
    q = parse_row_filters(row_filters),
    fields = parse_col_select(col_select)
  )

  # if no query input (i.e. only res_id)
  no_query <- is.null(query$q) && is.null(query$filter) && is.null(rows)
  # OR rows > 99999
  rows_in_range <- rows < 100000 || is.null(rows)
  # then use GET datastore_dump
  use_dump <- !rows_in_range || no_query

  if (use_dump) {

    # warn users that dump is being used, if user queried the data
    if (!no_query) {
      warning(
        "Queries matching more than 99,999 rows of data will return the full resource. Any row filters and/or column selections have been ignored. All rows and columns are now being downloaded.",
        call. = FALSE,
        immediate. = TRUE
      )
    }

    # fetch the data
    response <- httr::GET(url = ds_dump_url(res_id), user_agent = ua)
    httr::stop_for_status(response)
    stopifnot(httr::http_type(response) == "text/csv")

    # parse data
    data <- httr::content(response, "parsed") %>%
      dplyr::select(-"_id")

    return(data)

  } else {
    # if there is a query and rows < 99999
    # use datastore_search

    if (is.null(query$limit)) {
      # if there is no row limit set
      # set limit to CKAN default
      query$limit <- 100
    }

    # remove null values from query
    null_q_field <- sapply(query, is.null)
    query[null_q_field] <- NULL

    # define url to include the query
    url <- httr::modify_url(ds_search_url(),
      query = query
    )

    # fetch the data
    response <- httr::GET(url = url, user_agent = ua)
    httr::stop_for_status(response)
    stopifnot(httr::http_type(response) == "application/json")

    # parse the response
    parsed <- httr::content(response, "text") %>%
      jsonlite::fromJSON()

    # get the total number of records
    total_rows <- parsed$result$total

    if (is.null(rows) && query$limit < total_rows) {
      # if the total number of rows is greater than the
      # number of rows fetched from the datastore
      # AND the user was not aware of this limit (`rows` defaulted to NULL)
      # warn the user about this limit.
      warning(
        paste0("Returning the first ", query$limit,
            " results (rows) of your query. ",
            total_rows,
            " rows match your query in total. ",
            "To get ALL matching rows set the `rows` argument to ",
            total_rows, "."),
        call. = FALSE
      )
    }

    if (!is.null(rows) && query$limit > total_rows) {
      # if more rows were requested than received
      # let the user know
      message(
        paste0("You set `rows` to ", query$limit, ", but only ",
               total_rows, " rows matched your query.")
      )
    }

    data <- parsed$result$records %>%
      tibble::as_tibble() %>%
      # there is a strange behaviour of datastore_search that when
      # you filter on a field, e.g., {"Month":"202109"}
      # it returns an extra "rank" field, e.g., "rank Month"
      # temporary fix:
      dplyr::select(-dplyr::starts_with("rank "))

    return(data)
  }
}

#' Open Data user agent
#' @description
#' This is used internally to return a standard useragent
#' Supplying a user agent means requests using the package
#' can be tracked more easily
#'
#' @return a {httr} user_agent string
opendata_ua <- function() {
  httr::user_agent("https://github.com/Public-Health-Scotland/phsmethods")
}

#' Check if a resource ID is valid
#'
#' @description
#' Used to attempt to validate a res_id before submitting it to the API
#'
#' @param res_id a resource ID
#'
#' @return TRUE / FALSE indicating the validity of the res_id
#' @importFrom httr GET
check_res_id <- function(res_id) {

  res_id_regex <-
    "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"


  if (!inherits(res_id, "character")) {

    stop(
      "`res_id` should be of type character.",
      call. = FALSE
    )

  } else if (length(res_id) > 1) {

    stop(
      "`res_id` should be of length 1.
      `get_resource` does not currently support requests for multiple resources simultaneously.",
      call. = FALSE
    )

  } else if (!grepl(res_id_regex, res_id)) {

    stop(
      "`res_id` is not a valid resource id.",
      call. = FALSE
    )

  } else {
    # check if res_id is on the platform
    response <- GET(
      paste0(
        "https://www.opendata.nhs.scot/api/3/action/resource_show?id=",
        res_id
      )
    )

    if (response$status_code != 200) {
      stop(
        "`res_id` cannot be found on opendata.nhs.scot.",
        call. = FALSE
      )
    }

  }

}

#' Create json 'dict' from named list or vector
#' @description
#' Formats a list or named vector into a valid query
#'
#' @return a json as a character string
parse_row_filters <- function(row_filters) {

  if (is.null(row_filters)) {
    return(NULL)
  }

  # check if any filters in list have length > 1
  too_many <- sapply(row_filters, length) > 1

  if (any(too_many)) {
    stop(
      paste0(
        names(row_filters)[which(too_many)], " in `row_filters` has too many values. ",
        "The `row_filters` argument can only take vectors of length 1, e.g.:
        'K' or c('K'), not c('K', 'J')"
      ),
      .call = FALSE
    )
  }

  # check if any items in the list/vector have the same name
  unique_names <- length(unique(names(row_filters))) == length(names(row_filters))

  if (!unique_names) {
    stop(
      paste0(
        "One or more elements in `row_filters` have the same name. Only one filter per field is currently supported by `get_resource`.",
        call. = FALSE
      )
    )
  }

  filter_body <- paste0('"', names(row_filters), '":"', row_filters, '"', collapse = ",")

  return(
    paste0('{', filter_body, '}')
  )

}

#' Create fields query for GET
#' @description
#' Produces a comma separated list
#'
#' @return a character string
parse_col_select <- function(col_select) {

  if (is.null(col_select)) {
    return(NULL)
  } else {
    return(
      paste0(col_select, collapse = ",")
    )
  }

}


#' Creates the URL for the datastore search end-point
#'
#' @return a url
ds_search_url <- function() {
  httr::modify_url("https://www.opendata.nhs.scot",
    path = "/api/3/action/datastore_search"
  )
}

#' Creates the URL for the datastore dump end-point
#'
#' @param res_id a resource ID
#' @return a url
ds_dump_url <- function(res_id) {
  httr::modify_url("https://www.opendata.nhs.scot",
    path = glue::glue("/datastore/dump/{res_id}?bom=true")
  )
}
