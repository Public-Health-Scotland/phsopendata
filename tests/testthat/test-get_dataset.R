skip_if_offline(host = "www.opendata.nhs.scot")

test_that("returns data in the expected format", {
  n_resources <- 2
  n_rows <- 2
  data <- get_dataset(
    dataset_name = "gp-practice-populations",
    max_resources = n_resources,
    rows = n_rows
  )

  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), n_resources * n_rows)
  expect_length(data, 24)
  expect_named(data)
})

test_that("errors properly", {
  expect_error(get_dataset("Mal-formed-name"),
    regexp = "The dataset name supplied `Mal-formed-name` is invalid"
  )
  expect_error(get_dataset("dataset-name-with-no-close-match"),
    regexp = "Can't find the dataset name `dataset-name-with-no-close-match`"
  )
  expect_error(get_dataset("gp-practice-population"),
    regexp = "Did you mean .+?gp-practice-populations.+?\\?"
  )
})
