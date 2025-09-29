test_that("creates commas separated string from vector", {
  expect_equal(
    parse_col_select(letters[1:5]),
    "a,b,c,d,e"
  )
})

test_that("returns NULL if input is NULL", {
  expect_null(parse_col_select(NULL))
})

test_that("Can deal with a list in some cases", {
  expect_equal(
    parse_col_select(list("col1", "col2")),
    "col1,col2"
  )
  expect_equal(
    parse_col_select(list(columns = c("col1", "col2"))),
    "col1,col2"
  )
  expect_equal(
    parse_col_select(list(
      columns = c("col1", "col2"),
      columns2 = c("col1", "col2")
    )),
    "col1,col2"
  )

  expect_error(parse_col_select(list(1:3)))
})

test_that("errors on bad input", {
  expect_error(
    parse_col_select(1.0),
    "`col_select` must be a .+?character.+? vector, not a .+?numeric"
  )
  expect_error(
    parse_col_select(1:3),
    "`col_select` must be a .+?character.+? vector, not a .+?integer"
  )
  expect_error(
    parse_col_select(TRUE),
    "`col_select` must be a .+?character.+? vector, not a .+?logical"
  )
})
