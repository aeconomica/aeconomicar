test_that("Dataset", {
  res = fetch_dataset("WJP_STATE", vintage = "latest")
  expect_is(res, "data.frame")
  expect_true("dates" %in% colnames(res))
  expect_true("vintage" %in% colnames(res))
  expect_true("series_id" %in% colnames(res))
  expect_true("values" %in% colnames(res))
  expect_true("State" %in% colnames(res))

  res = fetch_dataset("WJP_STATE", restrictions = list("State" = c("TAS")))
  expect_is(res, "data.frame")
  expect_true("dates" %in% colnames(res))
  expect_true("vintage" %in% colnames(res))
  expect_true("series_id" %in% colnames(res))
  expect_true("values" %in% colnames(res))
  expect_true("State" %in% colnames(res))
  expect_true(unique(res$State) == c("TAS"))

  res = fetch_dataset("WJP_STATE", restrictions = list("State" = c("TAS")), dimensions = "name")
  expect_is(res, "data.frame")
  expect_true("dates" %in% colnames(res))
  expect_true("vintage" %in% colnames(res))
  expect_true("series_id" %in% colnames(res))
  expect_true("values" %in% colnames(res))
  expect_true("State" %in% colnames(res))
  expect_true(unique(res$State) == c("Tasmania"))

  test_that("Errors", {
    expect_error(fetch_dataset("*"), "`*` is not a valid series code", fixed = TRUE)
    expect_error(fetch_dataset("valid_but_nonexistent"), "DataSet `valid_but_nonexistent` does not exist", fixed = TRUE)
    expect_error(fetch_dataset("WJP_STATE", vintage = "abc"), "Invalid vintage `abc`. Options are a date (in form YYYY-MM-DD) or one of `current` (alias `latest`) or `previous`", fixed = TRUE)
    expect_error(fetch_dataset("WJP_STATE", dimensions = "fizz"), "`dimensions` can only be one of `code` or `name`", fixed = TRUE)
  })
})
