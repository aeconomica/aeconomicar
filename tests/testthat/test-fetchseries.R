test_that("Fetchseries", {

  res <- fetch_series("CPI")
  expect_is(res, "data.frame")
  expect_true(ncol(res) == 4)
  expect_true("dates" %in% colnames(res))
  expect_true("vintage" %in% colnames(res))
  expect_true("series_id" %in% colnames(res))
  expect_true("values" %in% colnames(res))

  res <- fetch_series("CPI", "latest")
  expect_is(res, "data.frame")
  expect_true(ncol(res) == 4)
  expect_true("dates" %in% colnames(res))
  expect_true("vintage" %in% colnames(res))
  expect_true("series_id" %in% colnames(res))
  expect_true("values" %in% colnames(res))

  res <- fetch_series(c("CPI", "CPI_SYD"), "latest")
  expect_is(res, "data.frame")
  expect_true(ncol(res) == 4)
  expect_true("dates" %in% colnames(res))
  expect_true("vintage" %in% colnames(res))
  expect_true("series_id" %in% colnames(res))
  expect_true("values" %in% colnames(res))

  res <- fetch_series(list("CPI" = "latest", "CPI_SYD" = "latest"))
  expect_is(res, "data.frame")
  expect_true(ncol(res) == 4)
  expect_true("dates" %in% colnames(res))
  expect_true("vintage" %in% colnames(res))
  expect_true("series_id" %in% colnames(res))
  expect_true("values" %in% colnames(res))

  test_that("Errors", {
    expect_error(fetch_series("*"), "`*` is not a valid series code", fixed = TRUE)
    expect_error(fetch_series("valid_but_nonexistent"), "No series `valid_but_nonexistent` exists", fixed = TRUE)
    expect_error(fetch_series("CPI", "abc"), "Invalid vintage `abc`. Options are a date (in form YYYY-MM-DD) or one of `current` (alias `latest`) or `previous`", fixed = TRUE)

    expect_error(fetch_series(c("CPI", "*")), "`*` is not a valid series code", fixed = TRUE)
    expect_error(fetch_series(c("CPI", "valid_but_nonexistent")), "No series `valid_but_nonexistent` exists", fixed = TRUE)
    expect_error(fetch_series(c("CPI", "CPI_SYD"), "abc"), "Invalid vintage `abc`. Options are a date (in form YYYY-MM-DD) or one of `current` (alias `latest`) or `previous`", fixed = TRUE)

    expect_error(fetch_series(list("CPI" = "latest", "*" = "latest")), "`*` is not a valid series code", fixed = TRUE)
    expect_error(fetch_series(list("CPI" = "latest", "valid_but_nonexistent" = "latest")), "No series `valid_but_nonexistent` exists", fixed = TRUE)
    expect_error(fetch_series(list("CPI" = "latest", "CPI_SYD" = "abc")), "Invalid vintage `abc`. Options are a date (in form YYYY-MM-DD) or one of `current` (alias `latest`) or `previous`", fixed = TRUE)
  })
})
