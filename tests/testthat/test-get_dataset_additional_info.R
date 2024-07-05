test_that("returns data in the expected format", {
  expect_s3_class(get_dataset_additional_info("weekly-accident-and-emergency-activity-and-waiting-times"), "tbl_df")
})

test_that("returned tibble has one row", {
  expect_equal(nrow(get_dataset_additional_info("weekly-accident-and-emergency-activity-and-waiting-times")), 1)
})
