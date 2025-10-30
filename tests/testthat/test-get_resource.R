skip_if_offline(host = "www.opendata.nhs.scot")

test_that("returns data in the expected format", {
  gp_list_apr_2021 <- "a794d603-95ab-4309-8c92-b48970478c14"

  # without query
  data <- get_resource(res_id = gp_list_apr_2021, rows = 1)

  expect_s3_class(data, "tbl_df")
  expect_gte(ncol(data), 8)
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

  expect_no_warning(get_resource("3e86b6fb-2062-4f05-8f4d-0bb001155d64"))
})

test_that("returns data with row specifications", {
  gp_list_apr_2021 <- "a794d603-95ab-4309-8c92-b48970478c14"

  expect_equal(nrow(get_resource(res_id = gp_list_apr_2021, rows = 926)), 926)

  expect_equal(
    nrow(get_resource(res_id = gp_list_apr_2021, rows = 999)),
    926
  ) %>%
    expect_warning()
})

test_that("returns data for multiple filters", {
  data_row_limit <- get_resource(
    res_id = "e4985a62-9d59-4e71-8800-3f7ca29ffe0c",
    col_select = c("GPPractice", "DMDCode"),
    row_filters = list("GPPractice" = c("80005", "80202")),
    rows = 100
  )

  expect_s3_class(data_row_limit, "tbl_df")
  expect_equal(nrow(data_row_limit), 100)
  expect_named(data_row_limit, c("GPPractice", "DMDCode"))

  data_full <- get_resource(
    res_id = "e4985a62-9d59-4e71-8800-3f7ca29ffe0c",
    col_select = c("GPPractice", "DMDCode", "PrescribedType"),
    row_filters = list(
      "GPPractice" = c("80005", "80202"),
      "PrescribedType" = "AMP"
    )
  )

  expect_s3_class(data_full, "tbl_df")
  expect_equal(nrow(data_full), 1108)
  expect_named(data_full, c("GPPractice", "DMDCode", "PrescribedType"))
  expect_length(unique(data_full$GPPractice), 2)
  expect_length(unique(data_full$PrescribedType), 1)
})

test_that("returns data for multiple filters in mixed format", {
  delays <- get_resource(
    res_id = "fd354e4b-6211-48ba-8e4f-8356a5ed4215",
    col_select = c("MonthOfDelay", "ReasonForDelay", "NumberOfDelayedBedDays"),
    row_filters = list("HBT" = "S08000031", MonthOfDelay = c(201607:201707))
  )

  expect_s3_class(delays, "tbl_df")
  expect_gte(nrow(delays), 13)
  expect_named(
    delays,
    c("MonthOfDelay", "ReasonForDelay", "NumberOfDelayedBedDays")
  )
  expect_length(unique(delays$MonthOfDelay), 13)
})

test_that("returns data for multiple filters for all columns", {
  prescriptions <- get_resource(
    res_id = "d1fbede3-98c4-436e-9e75-2ed807a36075",
    row_filters = list(
      "HBT" = "S08000015",
      "DMDCode" = c("940711000001101", "1004511000001101", "1014311000001109")
    )
  )

  expect_s3_class(prescriptions, "tbl_df")
  expect_equal(nrow(prescriptions), 114)
  expect_named(
    prescriptions,
    c(
      "HBT",
      "GPPractice",
      "DMDCode",
      "BNFItemCode",
      "BNFItemDescription",
      "PrescribedType",
      "NumberOfPaidItems",
      "PaidQuantity",
      "GrossIngredientCost",
      "PaidDateMonth"
    )
  )
  expect_length(unique(prescriptions$GPPractice), 55)
  expect_setequal(
    prescriptions$DMDCode,
    c("940711000001101", "1004511000001101", "1014311000001109")
  )
  expect_setequal(prescriptions$HBT, "S08000015")
})

test_that("errors on invalid filters", {
  # non-existent column in row_filters
  expect_error(
    delays <- get_resource(
      res_id = "fd354e4b-6211-48ba-8e4f-8356a5ed4215",
      col_select = c(
        "MonthOfDelay",
        "ReasonForDelay",
        "NumberOfDelayedBedDays"
      ),
      row_filters = c("HBT" = "S08000031", "Month" = 201607)
    ),
    regexp = "row_filters: invalid value"
  )

  # non-existent column in col_select
  expect_error(
    delays <- get_resource(
      res_id = "fd354e4b-6211-48ba-8e4f-8356a5ed4215",
      col_select = c("Month", "ReasonForDelay", "NumberOfDelayedBedDays"),
      row_filters = c("HBT" = "S08000031", "MonthOfDelay" = 201607)
    ),
    regexp = "col_select: invalid value"
  )
})

test_that("We can filter data with `Sex = 'All'`", {
  pops <- get_resource(
    res_id = "27a72cc8-d6d8-430c-8b4f-3109a9ceadb1",
    row_filters = list("Sex" = "All"),
    col_select = c("Year", "HB", "AllAges", "Sex")
  )

  expect_s3_class(pops, "tbl_df")
  expect_equal(
    nrow(pops),
    length(unique(pops$Year)) * 15
  )
  expect_named(
    pops,
    c(
      "Year",
      "HB",
      "AllAges",
      "Sex"
    )
  )
  expect_length(unique(pops$HB), 15)
  expect_setequal(pops$Sex, "All")
})
