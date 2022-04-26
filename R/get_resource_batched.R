#' Get a PHS open data resource in batches
#'
#' @description A wrapper around `get_resource()`.
#' It downloads whole resources a group of rows at a time in order to avoid API timeouts for large files.
#' You must assign a value to either `num_batches` or `rows_each`.
#'
#' @param query Should include at least a resource ID as found on
#' \href{https://www.opendata.nhs.scot/}{NHS Open Data platform}
#' @param num_batches (optional) sets the number of batches into which a download will be split
#' @param rows_each (optional) sets the number of rows to download in each batch
#' @param verbose Logical value. If TRUE, the function will print progress along the batches
#' as well as the estimated time remaining for all batches to complete. If FALSE no progress
#' or remaining time estimate will be reported.
#'
#' @seealso [get_resource()] for downloading resources without this batch mechanism.
#'
#' @importFrom magrittr %>%
#' @importFrom glue glue
#' @importFrom dplyr bind_rows
#' @return a [tibble][tibble::tibble-package] with the data
#' @export
#'
#' @examples get_resource_batched(query = list(id = "a794d603-95ab-4309-8c92-b48970478c14", filters = ...), num_batches = 10)
get_resource_batched <- function(query, num_batches = NULL, rows_each = NULL, verbose = FALSE) {

  res_id <- query$res_id

  if (is.null(query$filters) && is.null(query$fields)) {
    warning("You are attempting to download a full resource, using `get_resource()` instead.")
    return(
      get_resource(res_id)
    )
  }

  # stop if res_id is invalid
  if (!check_res_id(res_id)) stop(
    glue("The resource ID supplied ('{res_id}') is invalid")
  )

  # stop if num_batches and rows_each are defined
  if (is.null(num_batches) && is.null(rows_each)) stop(
    "Argument num_batches and rows_each are null objects. At least one must be a numeric value."
  )

  # stop if neither num_batches or rows_each are defined
  if (!is.null(num_batches) && !is.null(rows_each)) stop(
    "Please enter a value for `num_batches` OR `rows_each`, not both."
  )

  # if one of num_batches and rows_each is defined
  # calculate the value of the one that is NULL
  if (!is.null(num_batches) || !is.null(rows_each)) {
    # get total row number to calculate,
    total_rows <- get_total_records(res_id)

    if (!is.null(num_batches) && is.null(rows_each)) {
      # calculate rows_each
      total_rows <- get_total_records(res_id)
      rows_each <- ceiling(total_rows/num_batches)
    }

    if (is.null(num_batches) && !is.null(rows_each)) {
      # calculate num batches
      num_batches <- ceiling(total_rows/rows_each)
    }
  }

  # initialise batch list
  batches <- vector(mode = "list", length = num_batches)

  if (verbose) {
    time_estimates <- vector(length = num_batches)
  }

  # Loop along batches
  for (i in seq_along(batches)) {

    offset = (i-1)*rows_each
    over_total_rows <- offset > total_rows

    if (verbose & !over_total_rows) {
      t1 <- Sys.time()
      cat("Batch", i, "started\n")
    }

    if (!over_total_rows) {
      batches[[i]] <- get_resource(res_id, rows = rows_each, offset = offset)

      if (verbose) {
        t2 <- Sys.time()
        cat("Batch", i, "completed\n")
        time_estimates[i] <- difftime(t2, t1, units = "mins")
        cat(
          "\nEstimated time remaining (minutes):",
          mean(time_estimates[1:i])*(num_batches-i),
          "\n\n"
        )
      }
    } else {

      cat("Skipping batch", i, " because the entire resource is already downloaded\n")

    }


  }

  # join all batches together
  if (verbose) cat("Joining batches")
  data <- bind_rows(batches)

  return(data)

}

#' Find total number of rows for a resource
#'
#' @description
#'
#' @param res_id a resource ID
#' @importFrom jsonlite fromJSON
#' @return integer
get_total_records <- function(res_id) {
  return(
    fromJSON(
      paste0("https://www.opendata.nhs.scot/api/3/action/datastore_search?id=", res_id)
    )$result$total
  )
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
