test_that("Set APIKEY", {
  set_apikey("")
  expect_error(fetch_series("CPI"), "You have not set an `apikey`. Please run `set_apikey(\"YOURKEY\")` first.", fixed = TRUE)

  set_apikey(Sys.getenv("AECONOMICA_TEST_KEY"))
})
