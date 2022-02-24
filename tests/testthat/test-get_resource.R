test_that("returns data in the expected format", {
  gp_list_apr_2021 <- "a794d603-95ab-4309-8c92-b48970478c14"

  # without query
  data <- get_resource(res_id = gp_list_apr_2021, rows = 1)

  expect_s3_class(data, "tbl_df")
  expect_length(data, 15)
  expect_equal(nrow(data), 1)

  # with query
  data_q <- get_resource(
    gp_list_apr_2021,
    row_filters = list(PracticeCode = 10002),
    col_select = c("PracticeCode", "AddressLine1")
  )

  expect_true(all(names(data_q) == c("PracticeCode", "AddressLine1")))
  expect_true(all(data_q$PracticeCode == 10002))
})

test_that("checks res_id properly", {
  # wrong type
  expect_error(
    get_resource(res_id = 123),
    regexp = "*(should be of type character.)"
  )
  # Invalid format (doesn't match regex)
  expect_error(
    get_resource("a794d603-95ab-4309-8c92-b48970478c1"),
    regexp = "(is not a valid resource id.)"
  )
  # res_id is a vector of length > 1
  expect_error(
    get_resource(1:5),
    regexp = "(should be of length 1.)"
  )
  # Correct format but not real
  expect_error(
    get_resource("00000000-0000-0000-0000-000000000000"),
    regexp = "(cannot be found on opendata.nhs.scot.)"
  )
})

test_that("returns full data if only res_id is input", {
  gp_list_apr_2021 <- "a794d603-95ab-4309-8c92-b48970478c14"

  data <- get_resource(res_id = gp_list_apr_2021)

  expect_equal(nrow(data), 926)
})

test_that("returns full data if rows is set to over 99999", {
  gp_list_apr_2021 <- "a794d603-95ab-4309-8c92-b48970478c14"

  expect_warning(
    data <- get_resource(
      res_id = gp_list_apr_2021,
      rows = 100000
    ),
    regexp = "Queries matching more than 99,999 rows of data will return the full resource. Any row filters and/or column selections have been ignored. All rows and columns are now being downloaded."
  )

  expect_equal(nrow(data), 926)
})

test_that("first 99999 rows returned if query matches > 99999 rows", {
  prescriptions_apr_2021 <- "51b7ad3f-6d52-4165-94f4-92e322656c85"

  expect_warning(
    df <- get_resource(prescriptions_apr_2021, col_select = c("HBT")),
    regexp = "(Returning the first 99999 results (rows) of your query.)*"
  )

  expect_true(nrow(df) == 99999)

})
