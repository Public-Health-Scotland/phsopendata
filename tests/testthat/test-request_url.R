test_that("returns correct URL format", {
  expect_equal(
    phsopendata:::request_url("datastore_search", "id=doop"),
    "https://www.opendata.nhs.scot/api/3/action/datastore_search?id=doop"
  )

  expect_equal(
    phsopendata:::request_url("dump", "id=doop"),
    "https://www.opendata.nhs.scot/datastore/dump/id=doop?bom=true"
  )
})

test_that("rejects invalid actions", {
  expect_error(
    phsopendata:::request_url("beep", ""),
    regexp = "API call failed."
  )
})
