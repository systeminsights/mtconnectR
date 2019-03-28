
library("testthat")
library('plyr')
library('dplyr')

#===============================================================================
context("convert_ts_to_interval")

ts_data = data.frame(ts = as.POSIXct(c(0.5, 1, 1.008, 1.011) + 1445579573,  tz = 'CST6CDT', origin = "1970-01-01"),
                     x = c("a", "b", "c", "d"), y = c("e", "e", "e", "f"))
expected_interval = data.frame(start = ts_data$ts, end = c(ts_data$ts[2:4], ts_data$ts[1] + 10),
                               duration = c(0.500, 0.008, 0.003, 9.489),
                               x = c("a", "b", "c", "d"), y = c("e", "e", "e", "f"))
interval_data = convert_ts_to_interval(ts_data, time_colname = "ts", endtime_lastrow = ts_data$ts[1] + 10)
expect_equal(expected_interval, interval_data)

#===============================================================================
context("convert_interval_to_ts")

ts_reversed = convert_interval_to_ts(interval_data)
expect_equal(ts_reversed %>% dplyr::rename(ts = timestamp) %>% select(-duration), 
             rbind(ts_data, data.frame(ts = ts_data$ts[1] + 10, x = NA, y = NA)))

expect_equal(ts_data, ts_data %>% convert_ts_to_interval(time = "ts") %>%
               convert_interval_to_ts(remove_last = T) %>% dplyr::rename(ts = timestamp) %>% select(-duration))

expected_interval_disc = expected_interval[-2,]

expected_ts_disc = data.frame(timestamp = as.POSIXct(c(0.5, 1, 1.008, 1.011) + 1445579573,  tz = 'CST6CDT', origin = "1970-01-01"),
                              x = c("a", NA, "c", "d"), y = c("e", NA, "e", "f"))
expect_equal(expected_ts_disc, expected_interval_disc %>% convert_interval_to_ts(time_colname = "start", end_colname = "end", 
                                                                                 remove_last = T) %>% 
               select(-duration))


#===============================================================================
context("clean_redundant_rows")

test_interval = 
  data.frame(timestamp = as.POSIXct(c(0.5, 1, 1.008, 1.011),  tz = 'CST6CDT', origin = "1970-01-01"),
             x     = c("a", "b", "b", "b"), 
             y     = c("e", "e", "e", "f"))
expected_df = test_interval[c(1,2), ]
expect_equal(expected_df, clean_reduntant_rows(test_interval, "x"))

test_that("Returns df even for an input df with one column", {
   
  input_df = data.frame(col = c("A","A","B","C"))
  expected = data.frame(col = c("A","B","C"))
  expect_equal(expected, clean_reduntant_rows(input_df, "col"))
})

test_that("Cleans NA on 1 column", {
  
  input_df =  data.frame(a = c(1,2,2,3,3,NA,NA,4,5), b = c(1,2,2,99,99,99,3,4,5))
  expected_df = data.frame(a = c(1,2,3,NA,4,5), b = c(1,2,99,99,4,5))
  expect_equal(expected_df, clean_reduntant_rows(input_df, "a", clean_na = T))
})

test_that("Does Not Clean NA on 1 column", {
  
  input_df =  data.frame(a = c(1,2,2,3,3,NA,NA,4,5), b = c(1,2,2,99,99,99,3,4,5))
  expected_df = data.frame(a = c(1,2,3,NA,NA,4,5), b = c(1,2,99,99,3,4,5))
  expect_equal(expected_df, clean_reduntant_rows(input_df, "a", clean_na = F))
})

test_that("Cleans NA on 2 columns", {
  
  input_df =  data.frame(a = c(1,2,2,3,3,NA,NA,4,5), b = c(1,2,2,99,NA,NA,NA,4,5))
  expected_df = data.frame(a = c(1,2,3,3,NA,4,5), b = c(1,2,99,NA,NA,4,5))
  expect_equal(expected_df, clean_reduntant_rows(input_df, c("a", "b"), clean_na = T))
})

test_that("Does Not Clean NA on 2 columns", {
  
  input_df =  data.frame(a = c(1,2,2,3,3,NA,NA,4,5), b = c(1,2,2,99,NA,NA,NA,4,5))
  expected_df = data.frame(a = c(1,2,3,3,NA,NA,4,5), b = c(1,2,99,NA,NA,NA,4,5))
  expect_equal(expected_df, clean_reduntant_rows(input_df, c("a", "b"), clean_na = F))
})

test_that("Returns empty output for empty input", {
  
  input_df = data.frame(a = character(0), b = character(0), c = numeric(0))
  expected_df = data.frame(a = character(0), b = character(0), c = numeric(0))
  expect_equal(expected_df, clean_reduntant_rows(input_df, c("a", "b")))
})



#===============================================================================
context("grep_subset")

test_that("Filters correctly", {
  df = data.frame(type = c("sample","event","condition","sample"),
                  value = c("value1","value2","value3","value4"))
  filtered_df = grep_subset(df,"type","sample")
  expected_subset = data.frame(type = c("sample","sample"),value = c("value1","value4"))
  expect_equivalent(filtered_df,expected_subset)
})

test_that("Inverts correctly", {
  df = data.frame(type = c("sample","event","condition","sample"),
                  value = c("value1","value2","value3","value4"))
  filtered_df = grep_subset(df,"type","sample", invert = T)
  expected_subset = data.frame(type = c("event","condition"),value = c("value2","value3"))
  expect_equivalent(filtered_df,expected_subset)
})

test_that("Filters correctly for case insensitive", {
  df = data.frame(type = c("sample","event","condition","sample"),
                  value = c("value1","value2","value3","value4"))
  filtered_df = grep_subset(df, "type", stringr::fixed("Sample"))
  expected_subset = data.frame(type = c("sample","sample"),value = c("value1","value4"))[0,]
  expect_equivalent(filtered_df,expected_subset)
  
  df = data.frame(type = c("sample","event","condition","sample"),
                  value = c("value1","value2","value3","value4"))
  filtered_df = grep_subset(df, "type", stringr::fixed("Sample", ignore_case = T))
  expected_subset = data.frame(type = c("sample","sample"),value = c("value1","value4"))
  expect_equivalent(filtered_df,expected_subset)
})

