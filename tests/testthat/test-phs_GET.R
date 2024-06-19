skip_if_offline(host = "www.opendata.nhs.scot")

test_that("returns httr::content", {
  content <- phs_GET("package_list", "")

  expect_true(content$success)

  expect_equal(
    content$help,
    "https://www.opendata.nhs.scot/api/3/action/help_show?name=package_list"
  )
})

test_that("error_check() works as expected", {
  # no error for valid endpoint
  expect_type(
    phs_GET("package_list", ""),
    "list"
  )

  # not found error
  expect_error(
    phs_GET("datastore_search", "id=doop"),
    regexp = 'Resource "doop" was not found.'
  )
})

test_that("request_url() works as expected", {
  # invalid action argument
  expect_error(
    phs_GET("", ""),
    regexp = "API call failed"
  )
})
