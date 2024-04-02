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
  # check res_id
  check_res_id(res_id)

  # define query
  query <- list(
    id = res_id,
    limit = rows,
    q = parse_row_filters(row_filters),
    fields = parse_col_select(col_select)
  )

  # if dump should be used, use it
  if (use_dump_check(query, rows)) {
    return(dump_download(res_id))
  }

  # if there is no row limit set
  # set limit to CKAN max
  if (is.null(query$limit)) query$limit <- 99999

  # remove null values from query
  null_q_field <- sapply(query, is.null)
  query[null_q_field] <- NULL

  # fetch the data
  res_content <- phs_GET("datastore_search", query)

  # if the total number of rows is greater than the
  # number of rows fetched
  # AND the user was not aware of this limit (`rows` defaulted to NULL)
  # warn the user about this limit.
  total_rows <- res_content$result$total
  if (is.null(rows) && query$limit < total_rows) {
    cli::cli_warn(c(
      "Returning the first {query$limit}
      results (rows) of your query.
      {total_rows} rows match your query in total.",
      i = "To get ALL matching rows you will need to download
      the whole resource and apply filters/selections locally."
    ))
  }

  # if more rows were requested than received
  # let the user know
  if (!is.null(rows) && query$limit > total_rows) {
    cli::cli_alert_warning(c(
      "You set {.var rows} to {query$limit} but
      only {total_rows} rows matched your query."
    ))
  }

  # extract data from response content
  data <- purrr::map_dfr(
    res_content$result$records, ~.x
  ) %>% dplyr::select(
    -dplyr::starts_with("rank "),
    -dplyr::matches("_id")
  )

  return(data)
}
