# function to create a dummy resource catalogue for title-matching tests
resource_title_fixture <- function() {
  data.frame(
    resource_name = c(
      " Unique {title} ",
      "Shared {title}",
      "Shared {title}",
      paste("Example", seq_len(12)),
      NA_character_
    ),
    resource_id = sprintf(
      "00000000-0000-0000-0000-%012d",
      seq_len(16)
    ),
    dataset_name = paste0("dataset-", seq_len(16)),
    dataset_title = paste("Dataset", seq_len(16)),
    stringsAsFactors = FALSE
  )
}

# Test 1
test_that("invalid inputs pass through without catalogue lookup", {
  local_mocked_bindings(
    list_resources_query = function() {
      stop("Unexpected catalogue lookup")
    }
  )

  inputs <- list(
    NULL,
    123,
    character(),
    c("first", "second"),
    NA_character_
  )

  for (input in inputs) {
    expect_identical(
      resolve_resource_title_to_id(input),
      input
    )
  }
})

# Test 2
test_that("empty inputs and IDs avoid catalogue lookup", {
  local_mocked_bindings(
    list_resources_query = function() {
      stop("Unexpected catalogue lookup")
    }
  )

  id <- "a5d4de3f-e340-455f-b4e4-e26321d09207"

  expect_identical(resolve_resource_title_to_id(""), "")
  expect_identical(resolve_resource_title_to_id("   "), "")
  expect_identical(resolve_resource_title_to_id(id), id)
  expect_identical(
    resolve_resource_title_to_id(toupper(id)),
    toupper(id)
  )
  expect_identical(
    resolve_resource_title_to_id(paste0(" ", id, " ")),
    id
  )
})

# Test 3
test_that("unique titles resolve despite case and surrounding whitespace", {
  local_mocked_bindings(
    list_resources_query = resource_title_fixture
  )

  expect_warning(
    result <- resolve_resource_title_to_id(" unique {TITLE} "),
    "resolved to ID"
  )

  expect_identical(
    result,
    "00000000-0000-0000-0000-000000000001"
  )
})

# Test 4
test_that("ambiguous titles report both candidates with literal braces", {
  local_mocked_bindings(
    list_resources_query = resource_title_fixture
  )

  err <- expect_error(
    resolve_resource_title_to_id("Shared {title}"),
    "matched more than one resource"
  )

  message <- conditionMessage(err)

  expect_match(message, "Shared {title}", fixed = TRUE)
  expect_match(message, "Dataset 2", fixed = TRUE)
  expect_match(message, "Dataset 3", fixed = TRUE)
  expect_match(
    message,
    "00000000-0000-0000-0000-000000000002",
    fixed = TRUE
  )
  expect_match(
    message,
    "00000000-0000-0000-0000-000000000003",
    fixed = TRUE
  )
})

# Test 5
test_that("suggestions are limited to ten with the remainder reported", {
  local_mocked_bindings(
    list_resources_query = resource_title_fixture
  )

  err <- expect_error(
    resolve_resource_title_to_id("Example"),
    "did not match any resource title exactly"
  )

  message <- conditionMessage(err)

  for (i in seq_len(10)) {
    expect_match(
      message,
      paste0('"Example ', i, '"'),
      fixed = TRUE
    )
  }

  expect_false(grepl('"Example 11"', message, fixed = TRUE))
  expect_false(grepl('"Example 12"', message, fixed = TRUE))
  expect_match(message, "and 2 more", fixed = TRUE)
})

# Test 6
test_that("unmatched titles report no suggestions", {
  local_mocked_bindings(
    list_resources_query = resource_title_fixture
  )

  err <- expect_error(
    resolve_resource_title_to_id("zzzznomatch"),
    "did not match any resource title exactly"
  )

  expect_match(
    conditionMessage(err),
    "none found",
    fixed = TRUE
  )
})

# Test 7
test_that("suggestions match any input word without resolving automatically", {
  local_mocked_bindings(
    list_resources_query = resource_title_fixture
  )

  err <- expect_error(
    resolve_resource_title_to_id("zzzznomatch Example"),
    "did not match any resource title exactly"
  )

  expect_match(
    conditionMessage(err),
    '"Example 1"',
    fixed = TRUE
  )
})
