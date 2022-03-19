test_that("Fetchseries", {

  res <- fetch_series_as_at("CPI", "2021-01-03")
  expect_is(res, "data.frame")
  expect_true(ncol(res) == 4)
  expect_true("dates" %in% colnames(res))
  expect_true("vintage" %in% colnames(res))
  expect_true("series_id" %in% colnames(res))
  expect_true("values" %in% colnames(res))

  res <- fetch_series_as_at(c("CPI", "CPI_SYD"), "2021-01-03")
  expect_is(res, "data.frame")
  expect_true(ncol(res) == 4)
  expect_true("dates" %in% colnames(res))
  expect_true("vintage" %in% colnames(res))
  expect_true("series_id" %in% colnames(res))
  expect_true("values" %in% colnames(res))

  res <- fetch_series_as_at(list("CPI" = "2021-02-01", "CPI_SYD" = "2021-02-01"))
  expect_is(res, "data.frame")
  expect_true(ncol(res) == 4)
  expect_true("dates" %in% colnames(res))
  expect_true("vintage" %in% colnames(res))
  expect_true("series_id" %in% colnames(res))
  expect_true("values" %in% colnames(res))

  test_that("Errors", {
    expect_error(fetch_series_as_at("GDP"))

    expect_error(fetch_series_as_at("*", "2021-02-01"), "`*` is not a valid series code", fixed = TRUE)
    expect_error(fetch_series_as_at("valid_but_nonexistent", "2021-02-01"), "No series `valid_but_nonexistent` exists", fixed = TRUE)
    expect_error(fetch_series_as_at("CPI", "xxxx-yy-mm"), "Invalid as_at_date `xxxx-yy-mm`. Date must be in form `YYYY-MM-DD`.", fixed = TRUE)

    expect_error(fetch_series_as_at(c("CPI", "*"), "2021-02-01"), "`*` is not a valid series code", fixed = TRUE)
    expect_error(fetch_series_as_at(c("CPI", "valid_but_nonexistent"), "2021-02-01"), "No series `valid_but_nonexistent` exists", fixed = TRUE)
    expect_error(fetch_series_as_at(c("CPI", "CPI_SYD"), "xxxx-yy-mm"), "Invalid as_at_date `xxxx-yy-mm`. Date must be in form `YYYY-MM-DD`.", fixed = TRUE)

    expect_error(fetch_series_as_at(list("CPI" = "2021-02-01", "*" = "2021-02-01")), "`*` is not a valid series code", fixed = TRUE)
    expect_error(fetch_series_as_at(list("CPI" = "2021-02-01", "valid_but_nonexistent" = "2021-02-01")), "No series `valid_but_nonexistent` exists", fixed = TRUE)
    expect_error(fetch_series_as_at(list("CPI" = "2021-02-01", "CPI_SYD" = "xxxx-yy-mm")), "Invalid as_at_date `xxxx-yy-mm`. Date must be in form `YYYY-MM-DD`.", fixed = TRUE)
  })
})
