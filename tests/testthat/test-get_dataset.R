test_that("returns data in the expected format", {
  data <- get_dataset(
    dataset_name = "gp-practice-populations",
    max_resources = 2,
    rows = 2
  )

  expect_s3_class(data, "tbl_df")
  expect_length(data, 24)
  expect_equal(nrow(data), 2 * 2)
})

test_that("errors properly", {
  expect_error(get_dataset("Mal-formed-name"),
               regexp = "The dataset name supplied `Mal-formed-name` is invalid"
  )
  expect_error(get_dataset("dataset-name-with-no-close-match"),
               regexp = "Can't find the dataset name `dataset-name-with-no-close-match`"
  )
  expect_error(get_dataset("gp-practice-population"),
               regexp = "Did you mean 'gp-practice-populations'?")
})
