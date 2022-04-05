#' Get Open Data resources from a dataset
#'
#' @param dataset_name name of the dataset as found on
#' \href{https://www.opendata.nhs.scot/}{NHS Open Data platform}
#' @param max_resources (optional) the maximum number of resources
#' to return, use for testing code,
#' it will retunr the n latest resources
#' @param rows (optional) specify the max number of rows
#' to return for each resource.
#'
#' @seealso [get_resource()] for downloading a single resource
#' from a dataset.
#'
#' @importFrom magrittr %>%
#' @return a [tibble][tibble::tibble-package] with the data
#' @export
#'
#' @examples get_dataset("gp-practice-populations",
#'   max_resources = 2, rows = 10
#' )
get_dataset <- function(dataset_name, max_resources = NULL, rows = NULL) {

  # throw error if name type/format is invalid
  check_dataset_name(dataset_name)

  # define query and try API call
  query <- paste0("id=", dataset_name)
  content <- try(
    phs_GET("package_show", query), silent = TRUE
  )

  # if content contains a 'Not Found Error'
  # throw error with suggested dataset name
  if (grepl("Not Found Error", content[1]))
    suggest_dataset_name(dataset_name)

  # define list of resource IDs to get
  all_ids <- purrr::map_chr(content$result$resources, ~.x$id)
  n_res <- length(all_ids)
  res_index <- 1:min(n_res, max_resources)
  ids_selection <- all_ids[res_index]

  all_data <- purrr::map_dfr(
    ids_selection,
    get_resource,
    rows = rows
  )

  return(all_data)
}
