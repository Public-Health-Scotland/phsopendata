# keeping this 'offline' mocked test separate from test-get_resource.R, so it can
# run without a connection and not be skipped due to that file's skip_if_offline()
test_that("get_resource resolves titles before retrieving data", {
  id <- "00000000-0000-0000-0000-000000000001"
  # create a one-row data frame for the test
  catalogue <- data.frame(
    resource_name = "Unique resource",
    resource_id = id,
    dataset_name = "example-dataset",
    dataset_title = "Example dataset"
  )

  requests <- 0L

  local_mocked_bindings(
    list_resources_query = function() catalogue,
    use_dump_check = function(...) FALSE,
    phs_GET = function(action, query) {
      # update the request counter defined outside this mocked function
      requests <<- requests + 1L

      expect_identical(action, "datastore_search")
      expect_identical(query$id, id)
      expect_identical(query$limit, 8L)

      # return a dummy API response containing eight records
      list(
        result = list(
          total = 8L,
          records = lapply(
            seq_len(8),
            function(i) list(value = i)
          )
        )
      )
    }
  )

  expect_warning(
    by_title <- get_resource("Unique resource", rows = 8L),
    "resolved to ID"
  )

  expect_warning(
    by_id <- get_resource(id, rows = 8L),
    NA
  )

  expect_s3_class(by_title, "tbl_df")
  expect_identical(nrow(by_title), 8L)
  expect_identical(by_title, by_id)
  expect_identical(requests, 2L)
})
