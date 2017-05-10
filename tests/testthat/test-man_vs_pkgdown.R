context("compare_man_vs_pkgdown")

test_that("Functions in man are all in _pkgdown.yml", {
  # Avoid documenting testing internal functions, because they are in man but
  # not indexed
  exported_not_indexed <- list_internal_funs(raw_strings(path2r = "../../R/"))
  exported_not_indexed <- exported_not_indexed$fun_internal
  pkg_doc <- "forestr"
  not_applicable <- c(pkg_doc, exported_not_indexed)
  
  man <- dir("../../man") %>% 
  stringr::str_replace("\\.Rd", "") %>% 
  setdiff(not_applicable)
  
  pkg <- readr::read_lines("../../_pkgdown.yml") %>% 
  stringr::str_subset("^   -") %>% 
  stringr::str_replace("-", "") %>% 
  stringr::str_trim()
  
  expect_equal(sort(pkg),  sort(man))
  
  # expect_equal(setdiff(man, pkg), "character(0)")
  # expect_equal(setdiff(pkg, man), "character(0)")
  
  expect_true(purrr::is_empty(setdiff(man, pkg)))
  expect_true(purrr::is_empty(setdiff(pkg, man)))
  
  expect_true(purrr::is_empty(man[duplicated(man)]))
  expect_true(purrr::is_empty(pkg[duplicated(pkg)]))
})
