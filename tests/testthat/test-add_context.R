test_that("Returned context is the same for resource and dataset", {
  dataset <- get_dataset(
    "general-practitioner-contact-details",
    rows = 10,
    include_context = TRUE
  )

  res_id_1 <- "647b256e-4a03-4963-8402-bf559c9e2fff"
  resource_1 <- get_resource(
    res_id = res_id_1,
    rows = 10,
    include_context = TRUE
  )

  res_id_2 <- "e37c14fe-51f7-4935-87d1-c79b30fe8824"
  resource_2 <- get_resource(
    res_id = res_id_2,
    rows = 10,
    include_context = TRUE
  )

  # --- Remove from here
  # This code works around an issue with vctrs
  # https://github.com/r-lib/vctrs/issues/1930
  dataset_has_POSIXlt <- inherits(dataset$res_created_date, "POSIXlt")

  # If this test fails, that's good (probably) and this can all be removed
  expect_error(stopifnot(dataset_has_POSIXlt))

  if(!dataset_has_POSIXlt) {
  dataset <- dataset %>%
    dplyr::mutate(
      dplyr::across(c("res_created_date","res_modified_date"),
                      as.POSIXlt)
    )
  }
  # --- Remove to here

  expect_equal(
    dataset %>%
      dplyr::filter(res_id == res_id_1) %>%
      dplyr::select(!dplyr::where(~anyNA(.x))),
    resource_1,
    list_as_map = TRUE
  )
  expect_equal(
    dataset %>%
      dplyr::filter(res_id == res_id_2) %>%
      dplyr::select(!dplyr::where(~anyNA(.x))),
    resource_2,
    list_as_map = TRUE
  )
  # list_as_map = TRUE will sort variable names before comparing
})
