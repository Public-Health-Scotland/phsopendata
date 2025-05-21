test_that("returns data for a dataset that is listed", {
  expect_no_error(data <- get_latest_resource("gp-practice-populations"))
  expect_s3_class(data, "tbl_df")
  expect_match(data[["ResName"]], "^GP Practice Populations \\w+ [0-9]{4}$")

  expect_named(data, c(
    "ResID",
    "ResName",
    "ResCreatedDate",
    "ResModifiedDate",
    "Date",
    "PracticeCode",
    "HB",
    "HSCP",
    "Sex",
    "SexQF",
    "AllAges",
    "AllAgesQF",
    "Ages0to4",
    "Ages0To4QF",
    "Ages5to14",
    "Ages5To14QF",
    "Ages15to24",
    "Ages15To24QF",
    "Ages25to44",
    "Ages25To44QF",
    "Ages45to64",
    "Ages45To64QF",
    "Ages65to74",
    "Ages65To74QF",
    "Ages75to84",
    "Ages75To84QF",
    "Ages85plus",
    "Ages85PlusQF"
  ))
})

test_that("returns error for a dataset that is not listed", {
  expect_error(
    get_latest_resource("hospital-codes"),
    "not within the applicable datasets"
  )
})

test_that("errors for ambiguous resource", {
  expect_error(
    get_latest_resource_id("weekly-covid-19-statistical-data-in-scotland"),
    "The most recent id could not be identified"
  )
})
