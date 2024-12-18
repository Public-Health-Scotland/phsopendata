#' Use datastore dump to download whole files
#'
#' @param res_id a resource ID
#' @return dataframe containing resource records
#' @keywords internal
#' @noRd
dump_download <- function(res_id, call = rlang::caller_env()) {
  # fetch the data
  content <- suppressMessages(
    phs_GET("dump", res_id)
  )

  # if content is a web page
  if ("xml_document" %in% class(content)) {
    cli::cli_abort(
      c(
        "Can't find resource with ID {.var {res_id}} in datastore."
      ),
      call = call
    )
  }

  # return data
  return(content[, -1])
}
