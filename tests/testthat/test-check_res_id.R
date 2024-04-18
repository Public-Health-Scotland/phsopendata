test_that("errors for length > 1", {
  expect_error(
    check_res_id(letters),
    regexp = "You supplied a res_id with a length of 26"
  )
})

test_that("errors for incorrect type", {
  expect_error(
    check_res_id(20),
    regexp = "(must be of type character)"
  )
})

test_that("errors for invalid format", {
  expect_error(
    check_res_id("wrong format"),
    regexp = "is in an invalid format."
  )
})

test_that("returns nothing for correct format/length/type", {
  expect_equal(
    check_res_id("a965ee86-0974-4c93-bbea-e839e27d7085"),
    NULL
  )
})
