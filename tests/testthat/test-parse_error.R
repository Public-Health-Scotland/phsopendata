test_that("correctly extracts error from API response",  {
  content <- httr::content(
    httr::GET(
      phsopendata:::request_url("datastore_search", "id=doop")
    )
  )
  expect_equal(
    phsopendata:::parse_error(content$error),
    "Not Found Error: Not found: Resource \"doop\" was not found."
  )

  content <- httr::content(
    httr::GET(
      phsopendata:::request_url("datastore_search", "")
    )
  )
  expect_equal(
    phsopendata:::parse_error(content$error),
    "resource_id: Missing value"
  )

})
