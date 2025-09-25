skip_if_offline(host = "www.opendata.nhs.scot")

test_that("returns data in the expected format", {
  data <- list_resources("diagnostic-waiting-times")

  expect_s3_class(data, "tbl_df")
  expect_named(data, c("res_id", "name", "created", "last_modified"))
  expect_equal(dplyr::n_distinct(data[["res_id"]]), nrow(data))
  expect_equal(dplyr::n_distinct(data[["name"]]), nrow(data))
})

test_that("returns errors properly", {
  expect_error(
    list_resources(),
    "argument \"dataset_name\" is missing, with no default$"
  )
  expect_error(list_resources("bad_name"), "dataset_name must be in dash-case")
  expect_error(list_resources("incorrect-name"), "Can't find the dataset name")
  expect_error(
    list_resources("diagnostic-waiting-time"),
    "diagnostic-waiting-times"
  )
})
