test_that("errors for incorrect case", {
  expect_error(
    check_dataset_name("hospital codes"),
    regexp = "dataset_name must be in dash-case"
  )
})

test_that("errors for incorrect type", {
  expect_error(
    check_dataset_name(20),
    regexp = "dataset_name must be of type character"
  )
})

test_that("returns nothing for valid type and format", {
  expect_equal(
    check_dataset_name("hospital-codes"),
    NULL
  )
})
