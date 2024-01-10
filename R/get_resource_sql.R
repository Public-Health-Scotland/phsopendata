#' Get PHS Open Data using SQL
#'
#' @description Similar to [get_resource()]
#' but with more options for server-side querying of data.
#' However, this function has a lower maximum row number
#' (32,000 vs 99,999) for returned results.
#'
#' @param sql (character) a single PostgreSQL SELECT query.
#'
#' Must include a resource ID, which must be double-quoted
#' e.g.,
#'
#' ```SELECT * from "58527343-a930-4058-bf9e-3c6e5cb04010"```
#'
#'
#' Column names must be double-quoted,
#' while character values in filters must be single-quoted.
#' e.g.,
#'
#' ```"Age" = '34'```
#'
#'
#' You may need to escape quote marks with `\` to implement this. e.g.,
#'
#'
#' ```sql = "SELECT * FROM \"<res_id>\" WHERE \"Age\" = '34'"```.
#'
#' @seealso [get_resource()] for downloading a resource without using a SQL query.
#'
#' @importFrom magrittr %>%
#' @return a [tibble][tibble::tibble-package] with the query results.
#' Only 32,000 rows can be returned from a single SQL query.
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
#' cols <- c(
#'    "TotalCancelled", "TotalOperations",
#'    "Hospital", "Month"
#' )
#' row_filter <- c(Hospital = "D102H")
#'
#' df2 <- get_resource(
#'    "bcc860a4-49f4-4232-a76b-f559cf6eb885",
#'    col_select = cols,
#'    row_filters = row_filter
#' )
get_resource_sql <- function(sql) {

  if (length(sql) > 1)
    cli::cli_abort(c(
      "SQL validation error.",
      i = "{.var sql} must be of length 1",
      x = "You entered an object of length {length(sql)}."
    ))

  if (!("character" %in% class(sql)))
    cli::cli_abort(c(
      "SQL validation error.",
      i = "{.var sql} must be of class {.cls character}",
      x = "You entered an object of class {.cls {class(sql)[1]}}."
    ))

  # remove spaces
  sql <- gsub(" ", "", sql)
  sql <- gsub("\n", "", sql)

  # check query is a SELECT statement
  if (substr(sql, 1, 6) != "SELECT")
    cli::cli_abort(c(
      "SQL validation error.",
      i = "{.var sql} must start with SELECT"
    ))

  # add query field prefix
  query <- paste0("sql=", sql)

  # attempt get request
  content <- phs_GET("datastore_search_sql", query)

  # get correct order of columns
  order <- purrr::map_chr(
    content$result$fields,
    ~.x$id
  )

  # extract the records (rows) from content
  data <- purrr::map_dfr(
    content$result$records,
    ~{
       # replace NULL with "" so tibble works
       is_null <- purrr::map_lgl(.x, is.null)
       .x[is_null] <- ""

       tibble::as_tibble(.x)
     }
  )

  # select and reorder columns to reflect
  cleaner <- data %>%
    dplyr::select(
      dplyr::all_of(order),
      -dplyr::matches("_id"), -dplyr::matches("_full_text")
    )

  # warn if limit may have been surpassed
  if (nrow(cleaner) == 32000) {
    cli::cli_warn(c(
      "Row number limit",
      i = "SQL queries are limitted to returning 32,000 results.
      This may have effected the results of your query."
    ))
  }

  return(cleaner)

}

