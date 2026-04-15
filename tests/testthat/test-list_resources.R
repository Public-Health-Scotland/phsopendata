skip_if_offline(host = "www.opendata.nhs.scot")

all_data <- list_resources()

test_that("returns data in the expected format", {
  expect_s3_class(all_data, "tbl_df")
  expect_named(
    all_data,
    c(
      "resource_name",
      "resource_id",
      "dataset_name",
      "dataset_title",
      "dataset_id",
      "url",
      "last_modified"
    )
  )

  # Should have at least some rows and reasonable number of columns
  expect_gte(nrow(all_data), 1)
  expect_gte(ncol(all_data), 6)

  expect_type(all_data$resource_id, "character")
  expect_s3_class(all_data$last_modified, "POSIXct")
  expect_true(anyDuplicated(all_data$resource_id) == 0)
  expect_false(anyNA(all_data$resource_id))
  expect_false(anyNA(all_data$dataset_id))
})

test_that("filters by resource_contains (case-insensitive) and warns when empty", {
  # Pick a common term likely present across resources; adjust if needed
  data_eu <- list_resources(resource_contains = "european")
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
    data_none <- list_resources(
      resource_contains = "___no_such_resource___"
    ),
    regexp = "No resources found"
  )
  expect_equal(nrow(data_none), 0)
})

test_that("filters by dataset_contains (case-insensitive) and warns when empty", {
  data_pkg <- list_resources(dataset_contains = "hospital")
  expect_s3_class(data_pkg, "tbl_df")
  expect_gte(nrow(data_pkg), 0)

  name_ok <- grepl("hospital", data_pkg$dataset_name, ignore.case = TRUE)
  title_ok <- grepl("hospital", data_pkg$dataset_title, ignore.case = TRUE)

  expect_all_true(name_ok | title_ok)

  expect_warning(
    data_none <- list_resources(dataset_contains = "___no_such_package___"),
    regexp = "No resources found"
  )
  expect_identical(nrow(data_none), 0L)
})

test_that("combined filtering works", {
  # This test is intentionally permissive since the live catalogue changes.
  # Use broad terms that should commonly co-occur; tweak if it ever becomes too strict.
  data_combo <- list_resources(
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
      "dataset_title",
      "dataset_id",
      "url",
      "last_modified"
    )
  )

  name_ok <- grepl("hospital", data_combo$dataset_name, ignore.case = TRUE)
  title_ok <- grepl("hospital", data_combo$dataset_title, ignore.case = TRUE)

  expect_all_true(name_ok | title_ok)

  expect_match(
    object = data_combo$resource_name,
    regexp = "admissions",
    ignore.case = TRUE,
    all = TRUE
  )
})

test_that("empty/whitespace/NA filters are treated as NULL", {
  expect_identical(
    object = list_resources(resource_contains = ""),
    expected = all_data
  )
  expect_identical(
    object = list_resources(resource_contains = "   "),
    expected = all_data
  )
  expect_identical(
    object = list_resources(dataset_contains = ""),
    expected = all_data
  )
  expect_identical(
    object = list_resources(dataset_contains = "   "),
    expected = all_data
  )
  expect_identical(
    object = list_resources(dataset_contains = NA_character_),
    expected = all_data
  )
  expect_identical(
    object = list_resources(resource_contains = NA_character_),
    expected = all_data
  )
})


test_that("invalid regex patterns error clearly", {
  expect_error(
    list_resources(resource_contains = "("),
    "must be a valid regular expression"
  )
  expect_error(
    list_resources(dataset_contains = "("),
    "must be a valid regular expression"
  )
})


test_that("input validation: dataset_contains/resource_contains must be NULL or length-1", {
  # Length > 1 should error
  expect_error(
    list_resources(dataset_contains = c("a", "b")),
    regexp = "length 1 not length 2"
  )

  # Non character should error
  expect_error(
    list_resources(dataset_contains = TRUE),
    "must be .*character"
  )
  expect_error(
    list_resources(resource_contains = 123),
    "must be .*character"
  )
})

test_that("dataset_name argument is deprecated", {
  expect_warning(
    list_resources(dataset_name = "hospital"),
    class = "lifecycle_warning_deprecated"
  )
})

test_that("dataset_contains is AND-anywhere (any order): 'Flu covid' == 'covid flu'", {
  res1 <- list_resources(dataset_contains = "Flu covid")
  res2 <- list_resources(dataset_contains = "covid flu")

  titles1 <- sort(unique(res1$dataset_title))
  titles2 <- sort(unique(res2$dataset_title))

  expect_gt(length(titles1), 0L)
  expect_identical(titles1, titles2)

  combined <- paste(res1$dataset_name, res1$dataset_title)
  expect_all_true(grepl("flu", combined, ignore.case = TRUE))
  expect_all_true(grepl("covid", combined, ignore.case = TRUE))
})

test_that("dataset_contains supports multi-term queries like 'GP list'", {
  res <- list_resources(dataset_contains = "GP list")
  titles <- unique(res$dataset_title)

  expect_gt(length(titles), 0L)
  expect_true(any(grepl(
    "GP Practice Contact Details and List Sizes",
    titles,
    fixed = TRUE
  )))

  combined <- paste(res$dataset_name, res$dataset_title)
  expect_all_true(grepl("gp", combined, ignore.case = TRUE))
  expect_all_true(grepl("list", combined, ignore.case = TRUE))
})

test_that("single-term dataset_contains behaves like a normal search", {
  res <- list_resources(dataset_contains = "GP")
  titles <- sort(unique(res$dataset_title))

  expect_true(any(grepl(
    "GP Practice Population Demographics",
    titles,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "GP Practice Contact Details and List Sizes",
    titles,
    fixed = TRUE
  )))

  combined <- paste(res$dataset_name, res$dataset_title)
  expect_all_true(grepl("gp", combined, ignore.case = TRUE))
})

test_that("dataset_contains allows regex (alternation) inside tokens", {
  # Should behave like 'Flu covid' because covid|covid is trivial,
  # but demonstrates regex parsing works.
  res <- list_resources(dataset_contains = "Flu (covid|covid)")

  combined <- paste(res$dataset_name, res$dataset_title)
  expect_all_true(grepl("flu", combined, ignore.case = TRUE))
  expect_all_true(grepl("covid", combined, ignore.case = TRUE))
})

test_that("resource_contains supports multi-term AND-anywhere semantics", {
  res <- list_resources(resource_contains = "GP sizes")
  skip_if(nrow(res) == 0)

  expect_all_true(grepl("gp", res$resource_name, ignore.case = TRUE))
  expect_all_true(grepl("sizes", res$resource_name, ignore.case = TRUE))
})
