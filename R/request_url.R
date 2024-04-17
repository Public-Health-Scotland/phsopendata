#' Produces a URL for a GET request to opendata.nhs.scot
#'
#' @param action The API endpoint you want to use, e.g., "package_show" / "datastore_search".
#' @param query The query to pass to the endpoint defined by the action argument.
#' @return a URL as a character string
#'
request_url <- function(action, query) {
  # check action is valid
  valid_actions <- c("datastore_search", "datastore_search_sql", "dump", "package_show", "package_list")
  if (!(action %in% valid_actions)) {
    cli::cli_abort(c(
      "API call failed.",
      x = "Invalid {.var action} argument in request."
    ))
  }

  base_url <- "https://www.opendata.nhs.scot"

  if (action == "dump") {
    # return dump URL
    url <- httr::modify_url(
      url = base_url,
      path = c("datastore/dump", query),
      query = list(bom = "true")
    )
  } else {
    url <- httr::modify_url(
      url = base_url,
      path = c("api/3/action", action),
      query = query
    )
  }

  # return standard API endpoint (i.e., not dump)
  return(url)
}
