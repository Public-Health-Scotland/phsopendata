
test_that("returns httr::content", {

  x <- phsopendata:::phs_GET("package_list", "")
  expect_true(
    !is.null(x$help) && !is.null(x$success)
  )

})

test_that("error_check() works as expected",  {

  # no error for valid endpoint
  expect_type(
    phsopendata:::phs_GET("package_list", ""),
    "list"
  )

  # not found error
  expect_error(
    phsopendata:::phs_GET("datastore_search", "id=doop"),
    regexp = 'Resource "doop" was not found.'
  )

})

test_that("request_url() works as expected",  {

  # invalid action argument
  expect_error(
    phsopendata:::phs_GET("", ""),
    regexp = "API call failed"
  )

})
