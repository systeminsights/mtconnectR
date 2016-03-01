
library("testthat")
library(plyr)
library(dplyr)

data("example_mtc_device")

merged_device = merge(example_mtc_device)

#===============================================================================
ts_to_mtc_device(merged_device)

ts_to_mtc_device <- function(merged_device){
  data_item_names = setdiff(names(merged_device), "timestamp")
  data_item_list = lapply(data_item_names, function(x){
    temp_df = data.frame(merged_device$timestamp, merged_device[[x]])
  })
}