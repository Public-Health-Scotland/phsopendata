test_that("creates commas separated string from vector", {
  expect_equal(
    parse_col_select(letters[1:5]),
    "a,b,c,d,e"
  )
})

test_that("returns NULL if input is NULL", {
  expect_null(parse_col_select(NULL))
})
