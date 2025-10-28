skip_if_offline(host = "www.opendata.nhs.scot")

test_that("get_dataset returns data in the expected format", {
  n_resources <- 2
  n_rows <- 2
  data <- get_dataset(
    dataset_name = "gp-practice-populations",
    max_resources = n_resources,
    rows = n_rows
  )

  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), n_resources * n_rows)
  expect_named(data)
  expect_gte(ncol(data), 4)
})

test_that("get_dataset works properly with filters", {
  n_resources <- 3
  n_rows <- 10
  columns <- c("Date", "PracticeCode", "HSCP", "AllAges")

  data <- get_dataset(
    "gp-practice-populations",
    max_resources = n_resources,
    rows = n_rows,
    row_filters = list(HSCP = "S37000026"),
    col_select = columns
  )

  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), n_resources * n_rows)
  expect_named(data, columns)
  expect_true(all(data[["HSCP"]] == "S37000026"))
})

test_that("get_dataset errors properly", {
  expect_error(
    get_dataset("Mal-formed-name"),
    regexp = "The dataset name supplied `Mal-formed-name` is invalid"
  )
  expect_error(
    get_dataset("dataset-name-with-no-close-match"),
    regexp = "Can't find the dataset name `dataset-name-with-no-close-match`"
  )
  expect_error(
    get_dataset("gp-practice-population"),
    regexp = "Did you mean .+?gp-practice-populations.+?\\?"
  )
})

test_that("get_dataset filters error properly", {
  expect_error(
    get_dataset("gp-practice-populations", col_select = "Non-existent column"),
    regexp = "API error"
  )
})

test_that("get_dataset works with multiple filters", {
  n_resources <- 3
  columns <- c("Date", "PracticeCode", "HSCP", "AllAges")

  data <- get_dataset(
    "gp-practice-populations",
    max_resources = n_resources,
    row_filters = list(PracticeCode = c("10002", "10017")),
    col_select = columns
  )

  expect_s3_class(data, "tbl_df")
  expect_gte(nrow(data), n_resources * 4)
  expect_named(data, columns)
  expect_true(all(data[["PracticeCode"]] %in% c("10002", "10017")))
})

test_that("Warns when having to coerce types", {
  expect_warning(
    coerced_data <- get_dataset(
      dataset_name = "nhsscotland-payments-to-general-practice",
      rows = 1,
      col_select = "PracticeListSize"
    ),
    "Due to conflicts between column types across resources"
  )

  expect_s3_class(coerced_data, "tbl_df")
  expect_named(coerced_data, "PracticeListSize")
  expect_type(coerced_data[["PracticeListSize"]], "character")
})
