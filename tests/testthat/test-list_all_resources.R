skip_if_offline(host = "www.opendata.nhs.scot")

test_that("returns data in the expected format", {
  data <- list_all_resources()

  expect_s3_class(data, "tbl_df")
  expect_named(data)
  expect_true(all(c(
    "resource_name", "resource_id", "dataset_name", "dataset_id", "url", "last_modified"
  ) %in% names(data)))

  # Should have at least some rows and reasonable number of columns
  expect_gte(nrow(data), 1)
  expect_gte(ncol(data), 6)
})

test_that("filters by resource_contains (case-insensitive) and warns when empty", {
  # Pick a common term likely present across resources; adjust if needed
  data_eu <- list_all_resources(resource_contains = "european")
  expect_s3_class(data_eu, "tbl_df")
  expect_gte(nrow(data_eu), 0)
  if (nrow(data_eu) > 0) {
    expect_true(all(grepl("european", data_eu$resource_name, ignore.case = TRUE)))
  } else {
    expect_warning(list_all_resources(resource_contains = "___no_such_resource___"),
                   regexp = "No resources found"
    )
  }

  # Explicit empty case to assert warning + empty tibble
  expect_warning(
    data_none <- list_all_resources(resource_contains = "___no_such_resource___"),
    regexp = "No resources found"
  )
  expect_equal(nrow(data_none), 0)
})

test_that("filters by dataset_contains (case-insensitive) and warns when empty", {
  # Pick a common term likely present in package titles; adjust if needed
  data_pkg <- list_all_resources(dataset_contains = "hospital")
  expect_s3_class(data_pkg, "tbl_df")
  expect_gte(nrow(data_pkg), 0)
  if (nrow(data_pkg) > 0) {
    expect_true(all(grepl("hospital", data_pkg$dataset_name, ignore.case = TRUE)))
  } else {
    expect_warning(list_all_resources(dataset_contains = "___no_such_package___"),
                   regexp = "No packages found"
    )
  }

  # Explicit empty case to assert warning + empty tibble
  expect_warning(
    data_none <- list_all_resources(dataset_contains = "___no_such_package___"),
    regexp = "No packages found"
  )
  expect_equal(nrow(data_none), 0)
})

test_that("combined filtering works", {
  # This test is intentionally permissive since the live catalogue changes.
  # Use broad terms that should commonly co-occur; tweak if it ever becomes too strict.
  data_combo <- list_all_resources(
    dataset_contains = "hospital",
    resource_contains = "admissions"
  )

  expect_s3_class(data_combo, "tbl_df")
  expect_named(data_combo)
  expect_true(all(c(
    "resource_name", "resource_id", "dataset_name", "dataset_id", "url", "last_modified"
  ) %in% names(data_combo)))

  # If results exist, they should match both filters
  if (nrow(data_combo) > 0) {
    expect_true(all(grepl("hospital", data_combo$dataset_name, ignore.case = TRUE)))
    expect_true(all(grepl("admissions", data_combo$resource_name, ignore.case = TRUE)))
  }
})

test_that("input validation: dataset_contains/resource_contains must be NULL or length-1", {
  # Length > 1 should error
  expect_error(
    list_all_resources(dataset_contains = c("a", "b")),
    regexp = "length-1"
  )
  expect_error(
    list_all_resources(resource_contains = c("x", "y")),
    regexp = "length-1"
  )

  # Valid inputs should not error
  expect_no_error(list_all_resources(dataset_contains = NULL))
  expect_no_error(list_all_resources(resource_contains = NULL))
  expect_no_error(list_all_resources(dataset_contains = "hospital"))
  expect_no_error(list_all_resources(resource_contains = "admissions"))
})
