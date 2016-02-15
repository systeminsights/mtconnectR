.onLoad <- function(libname, pkgname) {
  options(stringsAsFactors = FALSE)
  options(digits.secs = 6)
  options(scipen = 4)
  options(digits = 10)
  data.table::setNumericRounding(0L)
}

Sys.setenv("TZ" = "UTC")
