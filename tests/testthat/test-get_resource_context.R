skip_if_offline(host = "www.opendata.nhs.scot")

test_that("returns expected context with the data", {
  gp_list_apr_2021 <- "a794d603-95ab-4309-8c92-b48970478c14"
  data_col_names <- names(get_resource(res_id = gp_list_apr_2021, rows = 1))

  # without query
  data <- get_resource(
    res_id = gp_list_apr_2021,
    rows = 10,
    include_context = TRUE
  )

  expect_s3_class(data, "tbl_df")
  expect_type(data$ResID, "character")
  expect_type(data$ResName, "character")
  expect_s3_class(data$ResCreatedDate, "POSIXct")
  expect_s3_class(data$ResModifiedDate, "POSIXct")

  expect_length(data, 19)
  expect_equal(nrow(data), 10)
  expect_named(
    data,
    c(
      "ResID",
      "ResName",
      "ResCreatedDate",
      "ResModifiedDate",
      data_col_names
    )
  )

  # with query
  data_q <- get_resource(
    gp_list_apr_2021,
    row_filters = list(PracticeCode = 10002),
    col_select = c("PracticeCode", "AddressLine1"),
    include_context = TRUE
  )

  expect_named(
    data_q,
    c(
      "ResID",
      "ResName",
      "ResCreatedDate",
      "ResModifiedDate",
      "PracticeCode",
      "AddressLine1"
    )
  )
  expect_equal(data_q[["PracticeCode"]], 10002)
})
