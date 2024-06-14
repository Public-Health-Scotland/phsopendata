skip_if_offline(host = "www.opendata.nhs.scot")

test_that("correctly extracts error from API response", {
  content <- httr::content(
    httr::GET(
      request_url("datastore_search", "id=doop")
    )
  )
  expect_equal(
    parse_error(content$error),
    "Not Found Error: Not found: Resource \"doop\" was not found."
  )

  content <- httr::content(
    httr::GET(
      request_url("datastore_search", "")
    )
  )
  expect_equal(
    parse_error(content$error),
    "resource_id: Missing value"
  )
})
