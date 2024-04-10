skip_if_offline(host = "www.opendata.nhs.scot")

test_that("returns data in the expected format", {
  gp_list_apr_2021 <- "a794d603-95ab-4309-8c92-b48970478c14"

  # without query
  data <- get_resource(res_id = gp_list_apr_2021, rows = 1)

  expect_s3_class(data, "tbl_df")
  expect_length(data, 15)
  expect_equal(nrow(data), 1)
  expect_named(data)

  # with query
  data_q <- get_resource(
    gp_list_apr_2021,
    row_filters = list(PracticeCode = 10002),
    col_select = c("PracticeCode", "AddressLine1")
  )

  expect_named(data_q, c("PracticeCode", "AddressLine1"))
  expect_equal(data_q[["PracticeCode"]], 10002)
})

test_that("returns expected context with the data", {
  gp_list_apr_2021 <- "a794d603-95ab-4309-8c92-b48970478c14"
  data_col_names <- names(get_resource(res_id = gp_list_apr_2021, rows = 1))

  # without query
  data <- get_resource(
    res_id = gp_list_apr_2021,
    rows = 1,
    include_context = TRUE
  )

  expect_s3_class(data, "tbl_df")
  expect_length(data, 19)
  expect_equal(nrow(data), 1)
  expect_named(data, c(
    "res_id",
    "res_name",
    "res_created_date",
    "res_modified_date",
    data_col_names
  ))

  # with query
  data_q <- get_resource(
    gp_list_apr_2021,
    row_filters = list(PracticeCode = 10002),
    col_select = c("PracticeCode", "AddressLine1"),
    include_context = TRUE
  )

  expect_named(data_q, c(
    "res_id",
    "res_name",
    "res_created_date",
    "res_modified_date",
    "PracticeCode",
    "AddressLine1"
  ))
  expect_equal(data_q[["PracticeCode"]], 10002)
})

test_that("checks res_id properly", {
  # wrong type
  expect_error(
    get_resource(res_id = 123),
    regexp = "(must be of type character)"
  )
  # Invalid format (doesn't match regex)
  expect_error(
    get_resource("a794d603-95ab-4309-8c92-b48970478c1"),
    regexp = "(is in an invalid format.)"
  )
  # res_id is a vector of length > 1
  expect_error(
    get_resource(1:5),
    regexp = "(must be of length 1.)"
  )
  # Correct format but not real
  expect_error(
    get_resource("00000000-0000-0000-0000-000000000000"),
    regexp = "(Can't find resource with ID)"
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
      rows = 9999999,
      row_filters = c("GPPracticeName" = "The Blue Practice")
    ),
    regexp = "Can't request over 99,999 rows"
  )

  expect_equal(nrow(data), 926)
})

test_that("first 99999 rows returned if query matches > 99999 rows", {
  prescriptions_apr_2021 <- "51b7ad3f-6d52-4165-94f4-92e322656c85"

  expect_warning(
    df <- get_resource(prescriptions_apr_2021, col_select = c("HBT")),
    regexp = "(Returning the first 99999 results)"
  )

  expect_true(nrow(df) == 99999)
})
