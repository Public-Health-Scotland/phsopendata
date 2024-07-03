test_that("returns more than 0 datasets", {
  # select the first row of the tibble and get the
  # number of rows. If no datasets were returned
  # this will be 0
  expect_equal(nrow(slice(list_datasets(), 1)), 1)
})
