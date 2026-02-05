test_that("Returned context is the same for resource and dataset", {
  skip_if_offline(host = "www.opendata.nhs.scot")

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

test_that("add_context works with odd data (offline)", {
  df <- tibble::tibble(
    Surname = letters[1:5],
    Sex = rep("Female", 5),
    Postcode = rep("EH1 1AA", 5),
    HB = rep("S08000030", 5),
    HSCP = rep("S37000026", 5)
  )

  res_id <- "647b256e-4a03-4963-8402-bf559c9e2fff"
  data <- add_context(
    data = df,
    id = res_id,
    name = "GP Details April 2024",
    created_date = "2025-01-01T00:00:00",
    modified_date = NULL
  )

  expect_s3_class(data, "tbl_df")
  expect_named(data)
  expect_contains(
    names(data),
    c(
      "ResID",
      "ResName",
      "ResCreatedDate",
      "ResModifiedDate",
      "Surname",
      "Sex",
      "Postcode",
      "HB",
      "HSCP"
    )
  )
  expect_true(all(is.na(data$ResModifiedDate)))
})

test_that("add_context() coerces modified_date >= created_date (offline)", {
  # 'modified_date' is slightly earlier than 'created_date' — function should correct it
  df <- tibble::tibble(A = 1:3, B = letters[1:3])

  created <- "2025-01-01T00:00:00"
  modified <- "2024-12-31T23:59:59" # earlier than created

  out <- add_context(
    data = df,
    id = "abc-123",
    name = "Example",
    created_date = created,
    modified_date = modified
  )

  expect_s3_class(out, "tbl_df")
  expect_true(all(out$ResModifiedDate >= out$ResCreatedDate))
  expect_equal(nrow(out), nrow(df))
})

test_that("add_context() prepends context columns in fixed order (offline)", {
  df <- tibble::tibble(x = 1:2, y = 3:4)

  out <- add_context(
    data = df,
    id = "abc-123",
    name = "Example",
    created_date = "2025-01-01T00:00:00",
    modified_date = "2025-01-01T00:00:01"
  )

  expect_identical(
    names(out)[1:4],
    c("ResID", "ResName", "ResCreatedDate", "ResModifiedDate")
  )
  expect_identical(names(out)[-(1:4)], names(df))
})
