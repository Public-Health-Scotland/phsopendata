test_that("throws error for non-existent res_ids", {

  expect_error(
    data <- phsopendata:::dump_download("not-real"),
    regexp = "Can't find resource with ID"
  )

})

test_that("downloads full resource", {

  data <- phsopendata:::dump_download("a965ee86-0974-4c93-bbea-e839e27d7085")

  expect_equal(nrow(data), 5)
  expect_equal(ncol(data), 10)

})
