test_that("returns correct URL format", {
  expect_identical(
    request_url("datastore_search", "id=doop"),
    "https://www.opendata.nhs.scot/api/3/action/datastore_search?id=doop"
  )

  expect_identical(
    request_url("dump", "id=doop"),
    "https://www.opendata.nhs.scot/datastore/dump/id=doop?bom=true"
  )
})

test_that("request_url() builds URLs for remaining valid actions", {
  # package_show
  expect_identical(
    request_url("package_show", list(id = "gp-practice-populations")),
    "https://www.opendata.nhs.scot/api/3/action/package_show?id=gp-practice-populations"
  )
  # resource_show
  expect_identical(
    request_url(
      "resource_show",
      list(id = "a794d603-95ab-4309-8c92-b48970478c14")
    ),
    "https://www.opendata.nhs.scot/api/3/action/resource_show?id=a794d603-95ab-4309-8c92-b48970478c14"
  )
  # datastore_search_sql
  expect_identical(
    request_url("datastore_search_sql", list(sql = 'SELECT * FROM "xyz"')),
    "https://www.opendata.nhs.scot/api/3/action/datastore_search_sql?sql=SELECT%20%2A%20FROM%20%22xyz%22"
  )
})


test_that("rejects invalid actions", {
  expect_error(
    request_url("beep", ""),
    regexp = "API call failed."
  )
})
