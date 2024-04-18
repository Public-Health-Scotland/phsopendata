skip_if_offline(host = "www.opendata.nhs.scot")

test_that("returns nothing if no error", {
  expect_null(
    error_check(phs_GET("package_list", ""))
  )
})

test_that("throws error if error in httr content", {
  expect_error(
    error_check(phs_GET("datastore_search", "id=doop")),
    regexp = 'Resource "doop" was not found.'
  )
})
