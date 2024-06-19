skip_if_offline(host = "www.opendata.nhs.scot")

test_that("throws error for non-existent res_ids", {
  expect_error(
    dump_download("not-real"),
    regexp = "Can't find resource with ID"
  )
})

test_that("downloads full resource", {
  data <- dump_download("a794d603-95ab-4309-8c92-b48970478c14")

  expect_equal(nrow(data), 926)
  expect_length(data, 15)
  expect_named(data)
  expect_s3_class(data, "tbl_df")
})
