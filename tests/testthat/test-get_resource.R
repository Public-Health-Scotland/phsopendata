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

test_that("returns data with row specifications", {
  gp_list_apr_2021 <- "a794d603-95ab-4309-8c92-b48970478c14"

  expect_equal(nrow(get_resource(res_id = gp_list_apr_2021, rows = 926)), 926)

  expect_equal(nrow(get_resource(res_id = gp_list_apr_2021, rows = 999)), 926) %>%
    expect_warning()
})

test_that("returns data for multiple filters", {
  expect_message(
    data_row_limit <- get_resource(
      res_id = "e4985a62-9d59-4e71-8800-3f7ca29ffe0c",
      col_select = c("GPPractice", "DMDCode"),
      row_filters = list("GPPractice" = c("80005", "80202")),
      rows = 100
    )
  )

  expect_s3_class(data_row_limit, "tbl_df")
  expect_equal(nrow(data_row_limit), 100)
  expect_named(data_row_limit, c("GPPractice", "DMDCode"))

  expect_message(
    data_full <- get_resource(
      res_id = "e4985a62-9d59-4e71-8800-3f7ca29ffe0c",
      col_select = c("GPPractice", "DMDCode", "PrescribedType"),
      row_filters = list(
        "GPPractice" = c("80005", "80202"),
        "PrescribedType" = "AMP"
      )
    )
  )

  expect_s3_class(data_full, "tbl_df")
  expect_equal(nrow(data_full), 1108)
  expect_named(data_full, c("GPPractice", "DMDCode", "PrescribedType"))
  expect_length(unique(data_full$GPPractice), 2)
  expect_length(unique(data_full$PrescribedType), 1)
})
