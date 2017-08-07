context("bci12 data is complete")

test_that("All census exist in data/", {
  library(dplyr)
  library(bci)
  all_censuses <- c(paste0("bci12full", 1:7), paste0("bci12stem", 1:7))
  all_censuses_exist <- lapply(all_censuses, exists) %>% 
    unlist() %>% 
    all()
  expect_true(all_censuses_exist)
})
