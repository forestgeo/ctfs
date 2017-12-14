context("bci12 data is complete")

test_that("All census exist in data/", {
  library(dplyr)
  library(bciex)
  all_censuses <- c(
    paste0("bci12t", 1:7, "mini"), 
    paste0("bci12s", 1:7, "mini")
  )
  all_censuses_exist <- lapply(all_censuses, exists) %>% 
    unlist() %>% 
    all()
  expect_true(all_censuses_exist)
})
