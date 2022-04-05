test_that("returns NULL if `row_filters` = NULL", {
  expect_null(
    phsopendata:::parse_row_filters(NULL)
  )
})

test_that("throws error for length > 1", {
  expect_error(
    phsopendata:::parse_row_filters(list(x = letters)),
    regexp = "(list must only contain vectors of length 1.)"
  )
})

test_that("throws error for non-unique names", {
  expect_error(
    phsopendata:::parse_row_filters(list(x = 1, x = 2)),
    regexp = "Only one filter per field is currently supported by `get_resource`"
  )
})

test_that("returns JSON string from list", {
  expect_true(
    jsonlite::validate(
      phsopendata:::parse_row_filters(list(x = 5, y = 6))
    )
  )
})
