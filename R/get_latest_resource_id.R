get_latest_resource_id <- function(dataset_name){
  applicable_datasets <- c("gp-practice-populations", "gp-practice-contact-details-and-list-sizes",
                           "nhsscotland-payments-to-general-practice", "dental-practices-and-patient-registrations",
                           "general-practitioner-contact-details", "prescribed-dispensed",
                           "prescriptions-in-the-community", "community-pharmacy-contractor-activity")

  # throw error if name type/format is invalid
  check_dataset_name(dataset_name)

  # define query and try API call
  query <- list("id" = dataset_name)
  content <- try(
    phs_GET("package_show", query),
    silent = TRUE
  )


  #check if data set is within applicable datasets
  #throw error if not
  if(!dataset_name %in% applicable_datasets){
    cli::cli_abort(c(
      "The dataset name supplied {.var {dataset_name}} is not within the applicable datasets",
      "x" = "Please see documentation.",
      "i" = "You can find dataset names in the URL
      of a dataset's page on {.url www.opendata.nhs.scot}."
    ))
  }

  # if content contains a 'Not Found Error'
  # throw error with suggested dataset name
  if (grepl("Not Found Error", content[1])) {
    suggest_dataset_name(dataset_name)
  }

  #send the api request
  query <- list("id" = dataset_name)
  content <- try(
    phs_GET("package_show", query),
    silent = TRUE
  )

  #retrieve the resource id's from returned contect
  all_ids <- purrr::map_chr(content$result$resources, ~ .x$id)


  #add the id, created date and last_modified to a dataframe
  id <- c()
  created_date <- c()
  modified_date <- c()

  for(i in content$result$resources){
    id <-  append(id, i$id)
    created_date <- append(created_date, i$created)
    modified_date <- append(modified_date, i$last_modified)
  }
  all_id_data <- tibble::tibble(id = id,
                      created_date = strptime(created_date, format = "%FT%X", tz = "UTC"),
                      modified_date = strptime(modified_date, format = "%FT%X", tz = "UTC")) %>%
    dplyr::mutate(most_recent_date_created = max(created_date))

  all_id_data_first_row <- all_id_data %>%
    dplyr::slice(1)

  if(all_id_data_first_row$created_date == all_id_data_first_row$most_recent_date_created){
    return(all_id_data_first_row$id)
  }else(warning("most recent id could not be identified"))



}







