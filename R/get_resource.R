#' Get Open Data resource
#'
#' @description Downloads a single resource from the NHS Open Data platform by resource ID, with optional filtering and column selection.
#'
#' @param res_id The resource ID as found on \href{https://www.opendata.nhs.scot/}{NHS Open Data platform} (character).
#' @param rows (optional) Maximum number of rows to return (integer).
#' @param row_filters (optional) A named list or vector specifying values of columns/fields to keep (e.g., list(Date = 20220216, Sex = "Female")).
#' @param col_select (optional) A character vector containing the names of desired columns/fields (e.g., c("Date", "Sex")).
#' @param include_context (optional) If `TRUE`, additional information about the resource will be added as columns to the data, including the resource ID, the resource name, the creation date, and the last modified/updated date.
#'
#' @seealso [get_dataset()] for downloading all resources from a given dataset.
#'
#' @return A [tibble][tibble::tibble-package] with the data.
#' @export
#'
#' @examples
#' res_id <- "ca3f8e44-9a84-43d6-819c-a880b23bd278"
#'
#' data <- get_resource(res_id)
#'
#' filters <- list("HB" = "S08000030", "Month" = "202109")
#' wanted_cols <- c("HB", "Month", "TotalPatientsSeen")
#'
#' filtered_data <- get_resource(
#'   res_id = res_id,
#'   row_filters = filters,
#'   col_select = wanted_cols
#' )
get_resource <- function(
  res_id,
  rows = NULL,
  row_filters = NULL,
  col_select = NULL,
  include_context = FALSE
) {
  # check res_id
  check_res_id(res_id)

  parsed_col_select <- parse_col_select(col_select)
  parsed_row_filters <- parse_row_filters(row_filters)

  if (is.logical(parsed_row_filters) && !parsed_row_filters) {
    if (!is.null(row_filters)) {
      col_select_sql <- dplyr::if_else(
        is.null(col_select),
        "*",
        paste0("\"", paste(col_select, collapse = "\",\""), "\"")
      )

      row_filters_sql <- paste(
        purrr::imap_chr(
          row_filters,
          function(value, col) {
            paste0("\"", col, "\"=\'", value, "\'", collapse = " OR ")
          }
        ),
        collapse = ") AND ("
      )

      sql <- sprintf(
        "SELECT %s FROM \"%s\" WHERE (%s) %s",
        col_select_sql,
        res_id,
        row_filters_sql,
        dplyr::if_else(is.null(rows), "", paste("LIMIT", rows))
      )

      return(get_resource_sql(sql))
    }
  }

  # define query
  query <- list(
    id = res_id,
    limit = rows,
    q = parsed_row_filters,
    fields = parsed_col_select
  )

  # if dump should be used, use it
  if (use_dump_check(query, rows)) {
    data <- dump_download(res_id)
  } else {
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
      cli::cli_warn(
        "You set {.var rows} to {.val {query$limit}} but only {.val {total_rows}} rows matched your query."
      )
    }

    # extract data from response content
    data <- purrr::map_dfr(
      res_content$result$records,
      ~.x
    ) %>%
      dplyr::select(
        -dplyr::starts_with("rank "),
        -dplyr::matches("_id")
      )
  }

  if (include_context) {
    # Get resource context if required
    context_content <- phs_GET(
      action = "resource_show",
      query = paste0("id=", res_id)
    )

    res_id <- context_content$result$id
    res_name <- context_content$result$name
    res_created_date <- context_content$result$created
    res_modified_date <- context_content$result$last_modified

    data <- data %>%
      add_context(
        id = res_id,
        name = res_name,
        created_date = res_created_date,
        modified_date = res_modified_date
      )
  }

  return(data)
}
