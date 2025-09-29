skip_if_offline(host = "www.opendata.nhs.scot")

test_that("returns data in the expected format", {
  dataset <- get_dataset_additional_info(
    "weekly-accident-and-emergency-activity-and-waiting-times"
  )

  expect_s3_class(dataset, "tbl_df")
  expect_equal(nrow(dataset), 1)
  expect_named(dataset, c("name", "n_resources", "last_updated"))
})
