.onLoad <- function(libname, pkgname) {
  options(stringsAsFactors = FALSE)
  options(digits.secs = 6)
  options(scipen = 4)
  options(digits = 10)
  data.table::setNumericRounding(0L)
}

# From hadley:
# Whenever you use C++ code in your package, 
# you need to clean up after yourself when your package is unloaded. 
# Do this by writing a .onUnload() function that unloads the DLL
.onUnload <- function(libpath) {
  library.dynam.unload("vayu", libpath)
}
