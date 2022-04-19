test_that("throws error and doesn't suggest for large distance", {
  expect_error(
    phsopendata:::suggest_dataset_name(
      "135987645892erhusidhnjsfdhf92ry9823hr2iuh2eiyrhwfue"
    ),
    regexp = "Can't find the dataset name"
  )
})

test_that("throws error and does suggest for close matches", {
  expect_error(
    phsopendata:::suggest_dataset_name(
      "rospital-codes"
    ),
    regexp = "Did you mean 'hospital-codes'?"
  )
})
