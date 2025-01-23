test_that("returns NULL if `row_filters` = NULL", {
  expect_null(
    parse_row_filters(NULL)
  )
})

test_that("throws error for bad types", {
  expect_error(
    parse_row_filters(mtcars),
    regexp = " must be a named .+? not a "
  )
  expect_error(
    parse_row_filters(c(x = TRUE)),
    regexp = " must be a named .+? not a "
  )
  expect_error(
    parse_row_filters(c(x = NA)),
    regexp = " must be a named .+? not a "
  )
  expect_error(
    parse_row_filters(c(x = as.Date("2000-01-01"))),
    regexp = " must be a named .+? not a "
  )
})

test_that("returns FALSE for length > 1", {
  expect_false(
    parse_row_filters(list(x = letters)),
  )
})

test_that("returns FALSE if `Sex = 'All'` is given", {
  expect_false(
    parse_row_filters(list("Sex" = "All"))
  )

  expect_false(
    parse_row_filters(list("Sex" = c("Male", "All")))
  )

  expect_false(
    parse_row_filters(list(
      "Sex" = "All",
      "HBT" = "S1234"
    ))
  )
})

test_that("throws error when un-named", {
  expect_error(
    parse_row_filters(list(1, 2)),
    regexp = " should be a named "
  )
  expect_error(
    parse_row_filters(list(a = 1, 2)),
    regexp = " should be a named "
  )
  expect_error(
    parse_row_filters(c(1, 2)),
    regexp = " should be a named "
  )
  expect_error(
    parse_row_filters(c(a = 1, 2)),
    regexp = " should be a named "
  )
})

test_that("throws error for non-unique names", {
  expect_error(
    parse_row_filters(list(x = 1, x = 2)),
    regexp = "Only one filter per field is currently supported"
  )
  expect_error(
    parse_row_filters(c(x = 1, x = 2)),
    regexp = "Only one filter per field is currently supported"
  )
})


test_that("returns JSON string from a named vector", {
  expect_true(
    jsonlite::validate(
      parse_row_filters(c(x = 5.0, y = 6.0))
    )
  )
  expect_true(
    jsonlite::validate(
      parse_row_filters(c(x = 5L, y = 6L))
    )
  )
  expect_true(
    jsonlite::validate(
      parse_row_filters(c(x = "a", y = "b"))
    )
  )
})

test_that("returns JSON string from list", {
  expect_true(
    jsonlite::validate(
      parse_row_filters(list(x = 5.0, y = 6.0))
    )
  )
  expect_true(
    jsonlite::validate(
      parse_row_filters(list(x = 5L, y = 6L))
    )
  )
  expect_true(
    jsonlite::validate(
      parse_row_filters(list(x = "a", y = "b"))
    )
  )
})
