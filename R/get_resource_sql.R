#' Get PHS Open Data using SQL
#'
#' @description Downloads data from the NHS Open Data platform using a SQL query. Similar to [get_resource()], but allows more flexible server-side querying. This function has a lower maximum row number (32,000 vs 99,999) for returned results.
#'
#' @param sql A single PostgreSQL SELECT query (character). Must include a resource ID, which must be double-quoted (e.g., `SELECT * from "58527343-a930-4058-bf9e-3c6e5cb04010"`).
#'
#' @seealso [get_resource()] for downloading a resource without using a
#' SQL query.
#'
#' @return A [tibble][tibble::tibble-package] with the query results. Only 32,000 rows can be returned from a single SQL query.
#' @export
#'
#' @examples
#' sql <- "
#'    SELECT
#'      \"TotalCancelled\",\"TotalOperations\",\"Hospital\",\"Month\"
#'    FROM
#'      \"bcc860a4-49f4-4232-a76b-f559cf6eb885\"
#'    WHERE
#'      \"Hospital\" = 'D102H'
#' "
#' df <- get_resource_sql(sql)
#'
#' # This is equivalent to:
#' cols <- c("TotalCancelled", "TotalOperations", "Hospital", "Month")
#' row_filter <- c(Hospital = "D102H")
#'
#' df2 <- get_resource(
#'   "bcc860a4-49f4-4232-a76b-f559cf6eb885",
#'   col_select = cols,
#'   row_filters = row_filter
#' )
get_resource_sql <- function(sql) {
  if (length(sql) != 1) {
    cli::cli_abort(c(
      x = "SQL validation error.",
      i = "{.var sql} must be length 1 not {length(sql)}."
    ))
  }

  if (!inherits(sql, "character")) {
    cli::cli_abort(c(
      x = "SQL validation error.",
      i = "{.var sql} must be of class {.cls character} not {.cls {class(sql)}}."
    ))
  }

  # check query is a SELECT statement
  if (!grepl("^\\s*?SELECT", sql)) {
    cli::cli_abort(c(
      x = "SQL validation error.",
      i = "{.var sql} must start with {.val SELECT}"
    ))
  }

  # Add the SQL statement to the query
  query <- list("sql" = sql)

  # attempt get request
  content <- phs_GET("datastore_search_sql", query)

  if (!is.null(content[["result"]][["records_truncated"]])) {
    cli::cli_warn(
      "The data was truncated because your query matched more than the
      maximum number of rows."
    )
  }

  # extract the records (rows) from content
  data <- purrr::map_dfr(
    content$result$records,
    ~ {
      # replace NULL with "" so tibble works
      is_null <- purrr::map_lgl(.x, is.null)
      .x[is_null] <- ""

      tibble::as_tibble(.x)
    }
  )

  # If the query returned no rows, exit now.
  if (nrow(data) == 0L) {
    return(data)
  }

  # get correct order of columns
  order <- purrr::map_chr(
    content$result$fields,
    ~ .x$id
  )
  order <- order[!order %in% c("_id", "_full_text")]

  # select and reorder columns to reflect
  cleaner <- dplyr::select(data, dplyr::all_of(order))

  # warn if limit may have been surpassed
  if (nrow(cleaner) == 32000L) {
    cli::cli_warn(c(
      "Row number limit",
      i = "SQL queries are limitted to returning 32,000 results.
      This may have affected the results of your query."
    ))
  }

  return(cleaner)
}
