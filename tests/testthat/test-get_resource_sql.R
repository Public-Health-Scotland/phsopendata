skip_if_offline(host = "www.opendata.nhs.scot")

test_that("throws errors on invalid sql argument", {
  # wrong class
  expect_error(
    get_resource_sql(9000),
    regexp = "must be of class"
  )

  # wrong length
  expect_error(
    get_resource_sql(letters),
    regexp = "must be length 1 not 26\\."
  )

  # wrong start
  expect_error(
    get_resource_sql("this is wrong"),
    regexp = "`sql` must start with"
  )
})

test_that("gets expected data for a simple SQL query", {
  data <- get_resource_sql(
    sql = "
     SELECT
       \"TotalCancelled\",\"TotalOperations\",\"Hospital\",\"Month\"
     FROM
       \"bcc860a4-49f4-4232-a76b-f559cf6eb885\"
     WHERE
       \"Hospital\" = 'D102H'
  "
  )

  expect_s3_class(data, "tbl")
  expect_equal(unique(data$Hospital), "D102H")
  expect_named(
    data,
    c("TotalCancelled", "TotalOperations", "Hospital", "Month")
  )
})

test_that("gets expected data for a joined SQL query", {
  data <- get_resource_sql(
    sql = paste(
      "SELECT pops.\"Year\", pops.\"HB\", lookup.\"HBName\", pops.\"AllAges\"",
      "FROM \"27a72cc8-d6d8-430c-8b4f-3109a9ceadb1\" AS pops",
      "JOIN \"652ff726-e676-4a20-abda-435b98dd7bdc\" AS lookup",
      "ON pops.\"HB\" = lookup.\"HB\"",
      "WHERE pops.\"Sex\" = 'All' AND pops.\"Year\" > 2006"
    )
  )

  expect_s3_class(data, "tbl")
  expect_gt(min(as.integer(data$Year)), 2006L)
  expect_named(data, c("Year", "HB", "HBName", "AllAges"))
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
