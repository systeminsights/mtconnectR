
library("testthat")

context("Common: Testing mergeTS Function")
test_that("It should merge the dataframes by taking the carry forwarding the last observation (locf)", {
  input <- list(df1 = data.frame(timestamp = c(0.5, 1, 1.008, 1.011) + 1445579573, x = c('a', 'b', 'c', 'd')),
                df2 = data.frame(timestamp = c(0.5, 1.011) + 1445579573, y = c('e', 'f')))
  
  expected1 <- data.frame(timestamp = c(0.5, 1, 1.008, 1.011) + 1445579573, x = c("a", "b", "c", "d"), y = c("e", "e", "e", "f"))
  expected2 <- data.frame(timestamp = c(0.5, 1, 1.008, 1.011) + 1445579573, df1 = c("a", "b", "c", "d"), df2 = c("e", "e", "e", "f"))
  
  expect_equal(mergeTS(DF_list = input, output_DF = TRUE, use_list_names = FALSE), expected1)
  expect_equal(mergeTS(DF_list = input, output_DF = TRUE, use_list_names = TRUE), expected2)
})