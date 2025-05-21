#' Send a GET request to the PHS CKAN API
#'
#' @inheritParams request_url
#' @param verbose TRUE or FALSE. If TRUE, a success message will be printed to the console.
#' @return content of a httr::GET request
#' @keywords internal
#' @noRd
phs_GET <- function(
  action,
  query,
  verbose = FALSE,
  call = rlang::caller_env()
) {
  # define URL
  url <- request_url(action, query)

  # Attempt GET request, gently retrying up to 3 times
  response <- httr::RETRY(
    verb = "GET",
    url = url,
    terminate_on = c(409),
    user_agent = httr::user_agent(
      "phsopendata (https://github.com/Public-Health-Scotland/phsopendata)"
    )
  )

  # Check for a response from the server
  if (!inherits(response, "response")) {
    cli::cli_abort(
      c(
        "Can't connect to the CKAN server.",
        i = "Check your network/proxy settings."
      ),
      call = call
    )
  }

  # Extract the content from the HTTP response
  if (httr::http_type(response) %in% c("text/html", "application/json")) {
    content <- httr::content(response)
  } else if (httr::http_type(response) == "text/csv") {
    content <- readr::read_csv(
      file = httr::content(response, as = "text"),
      guess_max = Inf
    )
  } else {
    cli::cli_abort(
      "The response contained an unhandled content type: {httr::http_type(response)}"
    )
  }

  # detect/handle errors
  error_check(content, call = call)

  if (verbose) cat("GET request successful.\n")
  return(content)
}
