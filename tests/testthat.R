
library(testthat)
library(aeconomicar)

set_apikey(Sys.getenv("AECONOMICA_TEST_KEY"))

test_check("aeconomicar")
