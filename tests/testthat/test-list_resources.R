skip_if_offline(host = "www.opendata.nhs.scot")

test_that("returns data in the expected format", {
  expect_s3_class(list_datasets(), "tbl_df")
})
