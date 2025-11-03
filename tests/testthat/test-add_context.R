skip_if_offline(host = "www.opendata.nhs.scot")

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

  expect_equal(
    dataset %>%
      dplyr::filter(ResID == res_id_1) %>%
      dplyr::select(!dplyr::where(~ anyNA(.x))),
    resource_1,
    # list_as_map = TRUE will sort variable names before comparing
    list_as_map = TRUE
  )
  expect_equal(
    dataset %>%
      dplyr::filter(ResID == res_id_2) %>%
      dplyr::select(!dplyr::where(~ anyNA(.x))),
    resource_2,
    list_as_map = TRUE
  )
})

test_that("add_context works with odd data", {
  res_id <- "647b256e-4a03-4963-8402-bf559c9e2fff"

  data <- get_resource(
    res_id = res_id,
    rows = 5
  ) %>%
    add_context(
      id = res_id,
      name = "GP Details April 2024",
      created_date = "2025-01-01T00:00:00",
      modified_date = NULL
    )

  expect_s3_class(data, "tbl_df")
  expect_named(data)
  expect_contains(names(data), c(
    "ResID", "ResName", "ResCreatedDate", "ResModifiedDate",
    "Surname", "Sex", "Postcode", "HB", "HSCP"
  ))
  expect_true(all(is.na(data$ResModifiedDate)))
})
