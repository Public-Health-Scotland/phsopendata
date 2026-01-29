skip_if_offline(host = "www.opendata.nhs.scot")

all_data <- list_all_resources()

test_that("returns data in the expected format", {
  expect_s3_class(all_data, "tbl_df")
  expect_named(
    all_data,
    c(
      "resource_name",
      "resource_id",
      "dataset_name",
      "dataset_id",
      "url",
      "last_modified"
    )
  )

  # Should have at least some rows and reasonable number of columns
  expect_gte(nrow(all_data), 1)
  expect_gte(ncol(all_data), 6)

  expect_type(all_data$resource_id, "character")
  expect_true(anyDuplicated(all_data$resource_id) == 0)
  expect_false(anyNA(all_data$resource_id))
  expect_false(anyNA(all_data$dataset_id))
})

test_that("filters by resource_contains (case-insensitive) and warns when empty", {
  # Pick a common term likely present across resources; adjust if needed
  data_eu <- list_all_resources(resource_contains = "european")
  expect_s3_class(data_eu, "tbl_df")
  expect_gte(nrow(data_eu), 0L)
  expect_match(
    object = data_eu$resource_name,
    regexp = "european",
    ignore.case = TRUE,
    all = TRUE
  )

  # Explicit empty case to assert warning + empty tibble
  expect_warning(
    data_none <- list_all_resources(
      resource_contains = "___no_such_resource___"
    ),
    regexp = "No resources found"
  )
  expect_equal(nrow(data_none), 0)
})

test_that("filters by dataset_contains (case-insensitive) and warns when empty", {
  data_pkg <- list_all_resources(dataset_contains = "hospital")
  expect_s3_class(data_pkg, "tbl_df")
  expect_gte(nrow(data_pkg), 0)
  expect_match(
    object = data_pkg$dataset_name,
    regexp = "hospital",
    ignore.case = TRUE,
    all = TRUE
  )

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
  expect_named(
    data_combo,
    c(
      "resource_name",
      "resource_id",
      "dataset_name",
      "dataset_id",
      "url",
      "last_modified"
    )
  )

  expect_match(
    object = data_combo$dataset_name,
    regexp = "hospital",
    ignore.case = TRUE,
    all = TRUE
  )
  expect_match(
    object = data_combo$resource_name,
    regexp = "admissions",
    ignore.case = TRUE,
    all = TRUE
  )
})

test_that("empty/whitespace/NA filters are treated as NULL", {
  expect_identical(
    object = list_all_resources(resource_contains = ""),
    expected = all_data
  )
  expect_identical(
    object = list_all_resources(resource_contains = "   "),
    expected = all_data
  )
  expect_identical(
    object = list_all_resources(dataset_contains = ""),
    expected = all_data
  )
  expect_identical(
    object = list_all_resources(dataset_contains = "   "),
    expected = all_data
  )
  expect_identical(
    object = list_all_resources(dataset_contains = NA_character_),
    expected = all_data
  )
  expect_identical(
    object = list_all_resources(resource_contains = NA_character_),
    expected = all_data
  )
})

test_that("input validation: dataset_contains/resource_contains must be NULL or length-1", {
  # Length > 1 should error
  expect_error(
    list_all_resources(dataset_contains = c("a", "b")),
    regexp = "length 1 not length 2"
  )
  expect_error(
    list_all_resources(resource_contains = c("x", "y")),
    regexp = "length-1"
  )
})
