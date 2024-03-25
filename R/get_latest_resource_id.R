#' Get the latest Open Data resource ID associated
#' with a dataset
#'
#' @description Returns the ID of the resource
#' that was uploaded to a dataset most recently.
#' This ID can be used as a parameter within
#' [get_resource()]
#'
#' @param dataset_name name of the dataset as found on
#' \href{https://www.opendata.nhs.scot/}{NHS Open Data platform}
#'
#' @importFrom magrittr %>%
#' @return a string of the resource ID that was uploaded to
#' the dataset most recently.
#'
#' @export


get_latest_resource_id <- function(dataset_name){
  ckanr::ckanr_setup("www.opendata.nhs.scot")

  check_dataset_name(dataset_name)

  # define query and try API call
  query <- paste0("id=", dataset_name)
  content <- try(
    phs_GET("package_show", query),
    silent = TRUE
  )

  # if content contains a 'Not Found Error'
  # throw error with suggested dataset name
  if (grepl("Not Found Error", content[1])) {
    suggest_dataset_name(dataset_name)
  }


  package <- ckanr::package_show(dataset_name)
  resources = c()
  updated_dates = c()
  resource_name = c()

  for (resource in package$resources){
    resources <-  append(resources, resource$id)
    updated_dates <- append(updated_dates, resource$last_modified)
    resource_name <- append(resource_name, resource$name)}


  package_resources <- tibble::tibble("resource_id" = resources, "updated_date" = updated_dates,
                              "resource_name" = resource_name)

  latest_resource <- package_resources %>%
    dplyr::filter(updated_date == max(updated_date))

  latest_resource_id <- latest_resource %>% dplyr::pull(resource_id)

  return(latest_resource_id)
}








