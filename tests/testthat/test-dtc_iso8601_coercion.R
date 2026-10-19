test_that("`create_iso8601()` values convert to base date-time classes", {
  x <- create_iso8601(c("20200102", "20200103"), .format = "ymd")

  expect_identical(as.Date(x), as.Date(c("2020-01-02", "2020-01-03")))
  expect_identical(
    as.POSIXct(x, tz = "UTC"),
    as.POSIXct(c("2020-01-02", "2020-01-03"), tz = "UTC")
  )
  expect_identical(
    as.POSIXlt(x, tz = "UTC"),
    as.POSIXlt(c("2020-01-02", "2020-01-03"), tz = "UTC")
  )
})

test_that("ISO 8601 date-time conversions preserve the time", {
  x <- create_iso8601("20200102", "1530", .format = c("ymd", "HM"))
  expected <- as.POSIXct("2020-01-02 15:30", tz = "UTC")

  expect_identical(as.Date(x), as.Date("2020-01-02"))
  expect_identical(as.POSIXct(x, tz = "UTC"), expected)
  expect_identical(as.POSIXlt(x, tz = "UTC"), as.POSIXlt(expected))
  expect_identical(as.POSIXct(x, tz = "UTC", format = "%Y-%m-%dT%H:%M"), expected)
  expect_identical(as.POSIXlt(x, tz = "UTC", format = "%Y-%m-%dT%H:%M"), as.POSIXlt(expected))
})

test_that("ISO 8601 conversions handle missing and empty values", {
  missing <- create_iso8601(c("20200102", NA_character_), .format = "ymd")
  empty <- structure(character(), class = "iso8601")

  expect_identical(as.Date(missing), as.Date(c("2020-01-02", NA_character_)))
  expect_identical(
    as.POSIXct(missing, tz = "UTC"),
    as.POSIXct(c("2020-01-02", NA_character_), tz = "UTC")
  )
  expect_identical(
    as.POSIXlt(missing, tz = "UTC"),
    as.POSIXlt(c("2020-01-02", NA_character_), tz = "UTC")
  )
  expect_identical(as.Date(empty), as.Date(character()))
  expect_identical(as.POSIXct(empty, tz = "UTC"), as.POSIXct(character(), tz = "UTC"))
  expect_identical(as.POSIXlt(empty, tz = "UTC"), as.POSIXlt(character(), tz = "UTC"))
})

test_that("converting ISO 8601 values preserves diagnostics on the source", {
  x <- create_iso8601(c("20200102", "invalid"), .format = "ymd", .warn = FALSE)
  original_problems <- problems(x)

  expect_identical(as.Date(x[1L]), as.Date("2020-01-02"))
  expect_s3_class(x, "iso8601")
  expect_identical(problems(x), original_problems)
  expect_identical(original_problems$..i, 2L)
})
