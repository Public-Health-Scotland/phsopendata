skip_if_offline(host = "www.opendata.nhs.scot")

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
      row_filters = list("GPPracticeName" = "The Blue Practice")
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
