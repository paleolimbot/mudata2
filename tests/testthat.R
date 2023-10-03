# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(mudata2)

verbose_test_output <- identical(tolower(Sys.getenv("R_MUDATA_VERBOSE_TEST", "false")), "true")

if (verbose_test_output) {
  reporter <- MultiReporter$new(list(CheckReporter$new(), LocationReporter$new()))
} else {
  reporter <- check_reporter()
}

test_check("mudata2", reporter = reporter)
