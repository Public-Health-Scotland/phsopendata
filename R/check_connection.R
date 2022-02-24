#' Check connection to CKAN API
#'
#'
#' @importFrom httr GET
#' @return doesn't return anything
check_connection <- function() {
  response <- GET(
    "https://www.opendata.nhs.scot/api/3/"
  )

  if (response$status_code != 200) {
    stop(
      "Connection to CKAN (https://www.opendata.nhs.scot/) failed.",
      call. = FALSE
    )
  }
}

