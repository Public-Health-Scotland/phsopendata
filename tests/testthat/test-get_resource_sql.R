skip_if_offline(host = "www.opendata.nhs.scot")

test_that("throws errors on invalid sql argument", {
  # wrong class
  expect_error(
    get_resource_sql(9000),
    regexp = "You entered an object of class <numeric>"
  )

  # wrong length
  expect_error(
    get_resource_sql(letters),
    regexp = "You entered an object of length 26."
  )

  # wrong start
  expect_error(
    get_resource_sql("this is wrong"),
    regexp = "`sql` must start with SELECT"
  )
})

test_that("gets expected data", {
  sql <- "
     SELECT
       \"TotalCancelled\",\"TotalOperations\",\"Hospital\",\"Month\"
     FROM
       \"bcc860a4-49f4-4232-a76b-f559cf6eb885\"
     WHERE
       \"Hospital\" = 'D102H'
  "
  df <- get_resource_sql(sql)

  expect_equal(unique(df$Hospital), "D102H")
  expect_equal(
    c("TotalCancelled", "TotalOperations", "Hospital", "Month"),
    names(df)
  )
})

test_that("SQL errors", {
  # non-existent column in real table
  expect_error(
    get_resource_sql(
      "SELECT \"Hospital\",\"donut\"
      from \"bcc860a4-49f4-4232-a76b-f559cf6eb885\""
    ),
    regexp = "column \"donut\" does not exist"
  )

  # non-existent table
  expect_error(
    get_resource_sql(
      "SELECT * from \"donut\""
    ),
    regexp = "resource/table \"donut\" does not exist"
  )

  # syntax errors
  expect_error(
    get_resource_sql(
      "SELECT * from 'donut'"
    ),
    regexp = "syntax error at or near \"'donut'\""
  )
})
