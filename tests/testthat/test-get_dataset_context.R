skip_if_offline(host = "www.opendata.nhs.scot")

test_that("returns expected context with the data", {
  n_rows <- 2
  data_col_names <- names(
    get_dataset(
      dataset_name = "gp-practice-populations",
      max_resources = 1,
      rows = 1
    )
  )
  data <- get_dataset(
    dataset_name = "gp-practice-populations",
    max_resources = 1,
    rows = n_rows,
    include_context = TRUE
  )

  expect_s3_class(data, "tbl_df")
  expect_type(data$ResID, "character")
  expect_type(data$ResName, "character")
  expect_s3_class(data$ResCreatedDate, "POSIXct")
  expect_s3_class(data$ResModifiedDate, "POSIXct")

  expect_equal(nrow(data), n_rows)
  expect_named(
    data,
    c("ResID", "ResName", "ResCreatedDate", "ResModifiedDate", data_col_names)
  )
})

test_that("returns expected context and length of data", {
  n_resources <- 2
  n_rows <- 2

  data <- get_dataset(
    dataset_name = "gp-practice-populations",
    max_resources = n_resources,
    rows = n_rows,
    include_context = TRUE
  )

  expect_s3_class(data, "tbl_df")
  expect_type(data$ResID, "character")
  expect_type(data$ResName, "character")
  expect_s3_class(data$ResCreatedDate, "POSIXct")
  expect_s3_class(data$ResModifiedDate, "POSIXct")

  expect_equal(nrow(data), n_resources * n_rows)
  expect_length(unique(data[["ResID"]]), n_resources)
})
