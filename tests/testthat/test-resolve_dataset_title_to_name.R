# These tests mock list_resources_query() rather than phs_GET() because
# list_resources_query() is memoised, and phs_GET() is only called if
# the memoise cache is empty

mock_catalogue <- function(...) {
  tibble::tribble(
    ~dataset_name, ~dataset_title,
    ...
  )
}

# Malformed input is returned unchanged without retrieving the dataset catalogue
test_that("resolve_dataset_title_to_name() passes malformed input through unchanged", {
  testthat::local_mocked_bindings(
    list_resources_query = function(...) {
      stop("list_resources_query() should not be called for malformed input")
    }
  )

  x_num <- 1
  x_vec <- c("a", "b")
  x_na <- NA_character_

  expect_identical(resolve_dataset_title_to_name(x_num), x_num)
  expect_identical(resolve_dataset_title_to_name(x_vec), x_vec)
  expect_identical(resolve_dataset_title_to_name(x_na), x_na)
})

# Input shaped like a dataset name is returned unchanged without accessing the
# catalogue
test_that("resolve_dataset_title_to_name() passes name-like input through unchanged", {
  testthat::local_mocked_bindings(
    list_resources_query = function(...) {
      stop("list_resources_query() should not be called for name-like input")
    }
  )

  expect_identical(
    resolve_dataset_title_to_name("gp-practice-populations"),
    "gp-practice-populations"
  )
})

# A unique exact title match is case-insensitive and produces a warning
test_that("resolve_dataset_title_to_name() resolves a unique exact title match", {
  catalogue <- mock_catalogue(
    "gp-practice-populations", "GP Practice Populations",
    "cancelled-operations", "Cancelled Operations"
  )

  testthat::local_mocked_bindings(
    list_resources_query = function(...) catalogue
  )

  # Use different capitalisation to confirm exact matching is case-insensitive
  expect_warning(
    out <- resolve_dataset_title_to_name("gp practice populations"),
    "resolved to name"
  )

  expect_identical(out, "gp-practice-populations")
})

# Identical ambiguous titles tie as the closest suggestions
test_that("identical ambiguous titles list both closest candidates", {
  catalogue <- mock_catalogue(
    "hospital-admissions-monthly", "Hospital Admissions",
    "hospital-admissions-quarterly", "Hospital Admissions",
    "hospital-codes", "Hospital Codes"
  )

  testthat::local_mocked_bindings(
    list_resources_query = function(...) catalogue
  )

  err <- rlang::catch_cnd(
    resolve_dataset_title_to_name("Hospital Admissions"),
    classes = "error"
  )

  expect_s3_class(err, "rlang_error")

  error_message <- conditionMessage(err)

  expect_match(error_message, "Can't find the dataset title")
  expect_match(error_message, "Did you mean")
  expect_match(error_message, "hospital-admissions-monthly")
  expect_match(error_message, "hospital-admissions-quarterly")

  # Ambiguous exact matches cannot be resolved uniquely, so both matching
  # dataset names are suggested
})

# A near-miss title returns the closest matching dataset as a suggestion
test_that("resolve_dataset_title_to_name() suggests a close title", {
  catalogue <- mock_catalogue(
    "gp-practice-populations", "GP Practice Populations",
    "cancelled-operations", "Cancelled Operations",
    "standard-populations", "Standard Populations"
  )

  testthat::local_mocked_bindings(
    list_resources_query = function(...) catalogue
  )

  err <- rlang::catch_cnd(
    resolve_dataset_title_to_name("GP Practice Population"),
    classes = "error"
  )

  expect_s3_class(err, "rlang_error")

  error_message <- conditionMessage(err)

  expect_match(error_message, "Can't find the dataset title")
  expect_match(error_message, "Did you mean")
  expect_match(error_message, "gp-practice-populations")
  expect_match(error_message, "GP Practice Populations")
})

# Titles with no close match return an error rather than a suggestion
test_that("resolve_dataset_title_to_name() reports when no title is close", {
  catalogue <- mock_catalogue(
    "gp-practice-populations", "GP Practice Populations",
    "cancelled-operations", "Cancelled Operations"
  )

  testthat::local_mocked_bindings(
    list_resources_query = function(...) catalogue
  )

  err <- rlang::catch_cnd(
    resolve_dataset_title_to_name("Completely Different Dataset"),
    classes = "error"
  )

  expect_s3_class(err, "rlang_error")

  error_message <- conditionMessage(err)

  expect_match(error_message, "Can't find the dataset title")
  expect_match(error_message, "or a close match")
  expect_match(error_message, "Find a dataset's name")
  expect_false(grepl("Did you mean", error_message, fixed = TRUE))
})

