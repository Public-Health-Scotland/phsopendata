#' Get Open Data resource
#'
#' @param res_id The resource ID as found on
#' \href{https://www.opendata.nhs.scot/}{NHS Open Data platform}
#' @param rows (optional) specify the max number of rows to return
#' use this when testing code to reduce the size of the request
#' it will default to all data
#' @param offset (optional) row in resource from which `rows` argument should start.
#' I.e., if `rows` = 100 and `offset` = 50, `get_resources()` with return row 51-100
#' of the resource.
#'
#' @seealso [get_dataset()] for downloading all resources
#' from a given dataset.
#'
#' @importFrom magrittr %>%
#' @importFrom httr GET modify_url user_agent stop_for_status http_type content
#' @importFrom tibble as_tibble
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#'
#' @return a [tibble][tibble::tibble-package] with the data
#' @export
#'
#' @examples get_resource(res_id = "a794d603-95ab-4309-8c92-b48970478c14")
get_resource <- function(res_id, rows = NULL, offset = NULL) {

  # check res_id is valid
  if (!check_res_id(res_id)) {
    stop(glue("The resource ID supplied ('{res_id}') is invalid"))
  }

  # if rows argument is null
  # set rows to large number
  # to ensure all records are shown
  if (is.null(rows)) {
    rows <- 999999999999999
    # `rows` needs to be larger than resource with most rows
  }

  # if offset argument is null
  # set offset to 0 to ensure
  # records shown start at 1
  if (is.null(offset)) {
    offset <- 0
  }

  # set resource id-s to use
  res_id <- res_id

  # Define the User Agent to be used for the API call
  ua <- opendata_ua()

  # define api query
  query <- list(
    id = res_id,
    limit = rows,
    offset = offset
  )

  url <- modify_url(ds_search_url(),
                          query = query
  )

  response <- GET(url = url, user_agent = ua)

  stop_for_status(response)

  stopifnot(http_type(response) == "application/json")

  parsed <- content(response, "text") %>%
    fromJSON()

  data <- parsed$result$records %>%
    as_tibble()

  return(data)

}

#' Open Data user agent
#' @description
#' This is used internally to return a standard useragent
#' Supplying a user agent means requests using the package
#' can be tracked more easily
#'
#' @return a {httr} user_agent string
opendata_ua <- function() {
  user_agent("https://github.com/Public-Health-Scotland/phsmethods")
}


#' Check if a resource ID is valid
#'
#' @description
#' Used to attempt to validate a res_id before submitting it to the API
#'
#' @param res_id a resource ID
#'
#' @return TRUE / FALSE indicating the validity of the res_id
check_res_id <- function(res_id) {
  res_id_regex <-
    "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"


  if (!inherits(res_id, "character")) {
    return(FALSE)
  } else if (!grepl(res_id_regex, res_id)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Creates the URL for the datastore search end-point
#'
#' @return a url
ds_search_url <- function() {
  modify_url("https://www.opendata.nhs.scot",
    path = "/api/3/action/datastore_search"
  )
}

#' Creates the URL for the datastore dump end-point
#'
#' @param res_id a resource ID
#' @return a url
ds_dump_url <- function(res_id) {
  modify_url("https://www.opendata.nhs.scot",
    path = glue("/datastore/dump/{res_id}?bom=true")
  )
}
