skip_if_offline(host = "www.opendata.nhs.scot")

test_that("returns expected context with the data", {
  n_resources <- 2
  n_rows <- 2
  data_col_names <- names(
    get_dataset(
      dataset_name = "gp-practice-populations",
      max_resources = 1,
      rows = 1
    )
  )
  data <- get_dataset(
    dataset_name = "gp-practice-populations",
    max_resources = n_resources,
    rows = n_rows,
    include_context = TRUE
  )

  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), n_resources * n_rows)
  expect_length(data, 28)
  expect_named(
    data,
    c("res_id", "res_name", "res_created_date", "res_modified_date", data_col_names)
  )
  expect_length(unique(data[["res_id"]]), n_resources)
})
