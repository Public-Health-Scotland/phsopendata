test_that("returns nothing if no error", {
  content <- httr::content(
    httr::GET(
      phsopendata:::request_url("package_list", "")
    )
  )
  expect_equal(
    phsopendata:::error_check(content), NULL
  )
})

test_that("throws error if error in httr content", {
  content <- httr::content(
    httr::GET(
      phsopendata:::request_url("datastore_search", "id=doop")
    )
  )
  expect_error(
    phsopendata:::error_check(content),
    regexp = 'Resource "doop" was not found.'
  )
})
