#' Send a GET request to the PHS CKAN API
#'
#' @inheritParams request_url
#' @param verbose TRUE or FALSE. If TRUE, a success message will be printed to the console.
#' @return content of a httr::GET request
phs_GET <- function(action, query, verbose = FALSE) {
  # define URL
  url <- request_url(action, query)

  # Attempt GET request, gently retrying up to 3 times
  response <- httr::RETRY(
    verb = "GET",
    url = url,
    user_agent = httr::user_agent(
      "https://github.com/Public-Health-Scotland/phsmethods"
    )
  )

  # Check for a response from the server
  if (!inherits(response, "response")) {
    cli::cli_abort(c(
      "Can't connect to the CKAN server.",
      i = "Check your network/proxy settings."
    ))
  }

  # Extract the content from the HTTP response
  content <- httr::content(
    response
  )

  # detect/handle errors
  error_check(content)

  if (verbose) cat("GET request successful.\n")
  return(content)
}
