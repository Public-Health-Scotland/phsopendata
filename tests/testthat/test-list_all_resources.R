test_that("list_all_resources() is deprecated with a message pointing to list_resources()", {
  skip_if_offline(host = "www.opendata.nhs.scot")
  expect_warning(
    list_all_resources(),
    class = "lifecycle_warning_deprecated"
  )
})
