skip_if_offline(host = "www.opendata.nhs.scot")

test_that("returns more than 0 datasets", {
  expect_gte(nrow(list_datasets()), 1)
})

test_that("returns data in the expected format", {
  data <- list_datasets()

  expect_s3_class(data, "tbl_df")
  expect_named(data, "name")
  expect_equal(dplyr::n_distinct(data[["name"]]), nrow(data))
})
