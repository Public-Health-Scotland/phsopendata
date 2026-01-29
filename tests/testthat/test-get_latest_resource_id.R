skip_if_offline(host = "www.opendata.nhs.scot")

test_that("returns data for a dataset that is listed", {
  gp_latest <- get_latest_resource("gp-practice-populations")

  expect_s3_class(gp_latest, "tbl")
  expect_gt(nrow(gp_latest), 1)
  expect_named(gp_latest)
  expect_contains(
    names(gp_latest),
    c("ResID", "ResName", "ResCreatedDate", "ResModifiedDate")
  )
  expect_s3_class(gp_latest$ResCreatedDate, "POSIXct")
  expect_s3_class(gp_latest$ResModifiedDate, "POSIXct")
})

test_that("returns error for a dataset that is not listed", {
  expect_error(
    get_latest_resource("hospital-codes"),
    "^The dataset name supplied.+?is not "
  )
})
