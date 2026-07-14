# This test suite mocks phs_GET() rather than calling the CKAN API. Beyond
# avoiding the need for real API calls, this allows testing for issues not
# currently present on the Scottish Health and Social Care Open Data platform.

# helper to nest lists created for the tests in Blocks 3-7 below under
# result$results, matching the structure of CKAN API output retrieved via phs_GET()
mock_package_search <- function(...) {
  list(
    result = list(
      results = list(...)
    )
  )
}

# Block 1: malformed input is returned unchanged
test_that("resolve_dataset_title_to_name() passes malformed input through unchanged", {
  testthat::local_mocked_bindings(
    phs_GET = function(...) {
      # any attempt to call phs_GET causes this test to fail
      stop("phs_GET() should not be called for malformed input")
    }
  )
  # tests for 3 kinds of malformed input
  x_num <- 1 # input is only a number
  x_vec <- c("a", "b") # input not a single character string (fails length==1)
  x_na <- NA_character_ # fails is.na check
  # confirm resolve_dataset_title_to_name returns these unchanged (for check_dataset_name)
  expect_identical(resolve_dataset_title_to_name(x_num), x_num)
  expect_identical(resolve_dataset_title_to_name(x_vec), x_vec)
  expect_identical(resolve_dataset_title_to_name(x_na), x_na)
})

# Block 2: input that looks like a dataset name passes through unchanged
test_that("resolve_dataset_title_to_name() passes name-like input through unchanged", {
  testthat::local_mocked_bindings(
    phs_GET = function(...) {
      stop("phs_GET() should not be called for name-like input")
    }
  )
  expect_identical(
    resolve_dataset_title_to_name("gp-practice-populations"),
    "gp-practice-populations"
  )
})

# Block 3: a single exact title match resolves to its name, and emits a warning
test_that("resolve_dataset_title_to_name() resolves a unique exact title match and warns", {
  mock_content <- mock_package_search(
    # lists below are nested to match the CKAN API output
    list(
      title = "GP Practice Populations",
      name = "gp-practice-populations"
    ),
    list(
      title = "Cancelled Operations",
      name = "cancelled-operations"
    )
  )
  # make mock API call
  testthat::local_mocked_bindings(
    phs_GET = function(action, query, ...) {
      expect_identical(action, "package_search")
      expect_identical(query$q, "*:*")
      expect_identical(query$rows, 10000)
      mock_content
    }
  )
  expect_warning(
    out <- resolve_dataset_title_to_name("gp practice populations"),
    "resolved to name"
  )
  expect_identical(out, "gp-practice-populations")
})

# Block 4: error if datasets share the same title (case insensitive)
test_that("resolve_dataset_title_to_name() errors on ambiguous exact title matches", {
  # construct mock API output
  mock_content <- mock_package_search(
    list(
      title = "Hospital Admissions",
      name = "hospital-admissions-monthly"
    ),
    list(
      title = "hospital admissions",
      name = "hospital-admissions-quarterly"
    ),
    list(
      title = "Cancelled Operations",
      name = "cancelled-operations"
    )
  )
  testthat::local_mocked_bindings(
    phs_GET = function(action, query, ...) {
      expect_identical(action, "package_search")
      expect_identical(query$q, "*:*")
      expect_identical(query$rows, 10000)
      mock_content
    }
  )
  err <- rlang::catch_cnd(
    resolve_dataset_title_to_name("Hospital Admissions"),
    classes = "error"
  )
  expect_s3_class(err, "rlang_error")
  # match on conditionMessage()
  expect_match(conditionMessage(err), "matched more than one dataset")
  expect_match(conditionMessage(err), "hospital-admissions-monthly")
  expect_match(conditionMessage(err), "hospital-admissions-quarterly")
})

# Block 5: no exact match, but error lists any candidates
test_that("resolve_dataset_title_to_name() errors on no exact match and shows substring candidates", {
  # construct mock API output
  mock_content <- mock_package_search(
    list(
      title = "GP Practice Populations",
      name = "gp-practice-populations"
    ),
    list(
      title = "GP Practice Contact Details",
      name = "gp-practice-contact-details"
    ),
    list(
      title = "Cancelled Operations",
      name = "cancelled-operations"
    )
  )
  # mock API call
  testthat::local_mocked_bindings(
    phs_GET = function(action, query, ...) {
      expect_identical(action, "package_search")
      expect_identical(query$q, "*:*")
      expect_identical(query$rows, 10000)
      mock_content
    }
  )
  # ensure errors are captured
  err <- rlang::catch_cnd(
    resolve_dataset_title_to_name("GP Practice"),
    classes = "error"
  )

  expect_s3_class(err, "rlang_error")
  expect_match(conditionMessage(err), "did not match any dataset title exactly")
  expect_match(conditionMessage(err), "gp-practice-populations")
  expect_match(conditionMessage(err), "gp-practice-contact-details")
})

# Block 6: no exact match AND no partial matches from substrings
test_that("resolve_dataset_title_to_name() errors on no exact match and no candidates", {
  mock_content <- mock_package_search(
    list(
      title = "GP Practice Populations",
      name = "gp-practice-populations"
    ),
    list(
      title = "Cancelled Operations",
      name = "cancelled-operations"
    )
  )
  testthat::local_mocked_bindings(
    phs_GET = function(action, query, ...) {
      expect_identical(action, "package_search")
      expect_identical(query$q, "*:*")
      expect_identical(query$rows, 10000)
      mock_content
    }
  )
  err <- rlang::catch_cnd(
    resolve_dataset_title_to_name("Completely Different Dataset"),
    classes = "error"
  )

  expect_s3_class(err, "rlang_error")
  expect_match(conditionMessage(err), "did not match any dataset title exactly")
  expect_match(conditionMessage(err), "Candidates: none")
})

# Block 7: missing names or titles in retrieved data are ignored
test_that("resolve_dataset_title_to_name() ignores results with missing title or name", {
  mock_content <- mock_package_search(
    list(
      title = "GP Practice Populations",
      name = NULL
    ),
    list(
      title = NULL,
      name = "gp-practice-populations-old"
    ),
    list(
      title = "GP Practice Populations",
      name = "gp-practice-populations"
    )
  )
  testthat::local_mocked_bindings(
    phs_GET = function(action, query, ...) {
      expect_identical(action, "package_search")
      expect_identical(query$q, "*:*")
      expect_identical(query$rows, 10000)
      mock_content
    }
  )
  expect_warning(
    out <- resolve_dataset_title_to_name("GP Practice Populations"),
    "resolved to name"
  )
  expect_identical(out, "gp-practice-populations")
})
