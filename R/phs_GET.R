#' Send a GET request to the PHS CKAN API
#'
#' @param action The API endpoint you want to use, e.g., "package_show" / "datastore_search".
#' @param query The query to pass to the endpoint defined by the action argument.
#' @param verbose TRUE or FALSE. If TRUE, a success message will be printed to the console.
#' @return content of a httr::GET request
#'
phs_GET <- function(action, query, verbose = FALSE) {

  # define URL
  url <- request_url(action, query)

  # attempt GET request
  response <- httr::GET(
    url,
    user_agent = httr::user_agent(
      "https://github.com/Public-Health-Scotland/phsmethods"
    )
  )

  # Check for response from server
  if (!inherits(response, "response")) {
    cli::cli_abort(c(
      "Can't connect to the CKAN server.",
      i = "Check your network/proxy settings."
    ))
  }

  # extract content from HTTP response
  content <- httr::content(
    response
  )

  # detect/handle errors
  error_check(content)

  if (verbose) cat("GET request successful.\n")
  return(content)
}
