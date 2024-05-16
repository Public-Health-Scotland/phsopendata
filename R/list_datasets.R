#' Lists all available datasets
#'
#' `list_datasets()` shows all of the datasets hosted on the phs open data platform.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' head(list_datasets())
#'
list_datasets <- function(include_additional_info = F) {
  data_sets <- phs_GET("package_list", "")$result
  if(!include_additional_info){
    data_sets%>%
    unlist()

  return(data_sets)}
  else{
    datasets <- purrr::map(data_sets, get_datasets_additional_info)

    return_value <- tibble::tibble(name = purrr::map_chr(datasets, ~.$name),
                                   most_recent_resource_date = purrr::map_chr(datasets, ~.$most_recent_resource_date),
                                   amount_of_resources = purrr::map_int(datasets, ~.$amount_of_resources)) %>%
      dplyr::mutate(most_recent_resource_date = as.POSIXct(most_recent_resource_date, format = "%FT%X", tz = "UTC"))

    return(return_value)
  }
}






