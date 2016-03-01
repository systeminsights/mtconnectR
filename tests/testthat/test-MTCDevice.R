
library("testthat")
library(plyr)
library(dplyr)

data("example_mtc_device")

merged_device = merge(example_mtc_device)

#===============================================================================
mtc_device_unmerged = ts_to_mtc_device(merged_device)
mtc_device_remerged = merge(mtc_device_unmerged)
expect_equal(merged_device, mtc_device_remerged)
