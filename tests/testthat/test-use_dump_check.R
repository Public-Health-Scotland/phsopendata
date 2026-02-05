test_that("returns true as expected", {
  # all are null
  expect_true(
    use_dump_check(list(), NULL)
  )

  # rows > 99999 and all query is NULL
  expect_true(
    use_dump_check(list(), 100000L)
  ) %>%
    expect_warning()

  # query entries are not all NULL and rows > 99999
  expect_true(
    use_dump_check(list(q = 4L), 100000L)
  ) %>%
    expect_warning()

  expect_true(
    use_dump_check(list(q = 4L), 100000L)
  ) %>%
    expect_warning()
})

test_that("returns false as expected", {
  # rows is NULL and query list is not all NULL
  expect_false(
    use_dump_check(list(fields = "Age"), NULL)
  )

  # rows is under 99999 and query list is not all NULL
  expect_false(
    use_dump_check(list(fields = "Age"), 100L)
  )

  # rows is under 99999 and query list is all NULL
  expect_false(
    use_dump_check(list(), 100L)
  )
})
