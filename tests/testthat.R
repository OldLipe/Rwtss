library(testthat)
library(Rwtss)

if (Sys.getenv("RWTSS_TESTS", unset = 0) == 1) {
    test_check("Rwtss")
}
