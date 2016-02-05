
library("testthat")

context("find_line_type")
log_data = c("*This is a command",
             "2015-01-01T07:07:07|This is time series",
             "2015-01-01T07:07:07|@ASSET@|",
             "Unknown Type")

expect_equal(vapply(log_data, find_line_type, ""), c("COMMAND", "TS", "ASSET", "UNKNOWN"))

