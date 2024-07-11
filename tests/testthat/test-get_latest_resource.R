test_that("returns more than 0 datasets", {
  # select the first row of the tibble and get the
  # number of rows. If no datasets were returned
  # this will be 0
  expect_equal(nrow(dplyr::slice(get_latest_resource("gp-practice-populations"), 1)), 1)
})

test_that("returns data in the expected format", {
  expect_s3_class(get_latest_resource("gp-practice-populations"), "tbl_df")
})
