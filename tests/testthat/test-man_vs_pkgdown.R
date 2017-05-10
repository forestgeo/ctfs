context("compare_man_vs_pkgdown")

test_that("Functions in man are all in _pkgdown.yml", {
  exported_not_indexed <- c(
  "rm_na_row", 
  "wsgdata_dummy", 
  "is_na_row"
  )
  pkg_doc <- "forestr"
  not_applicable <- c(pkg_doc, exported_not_indexed)
  
  man <- dir("./man") %>% 
  stringr::str_replace("\\.Rd", "") %>% 
  setdiff(not_applicable)
  
  pkg <- readr::read_lines("../../_pkgdown.yml") %>% 
  # pkg <- readr::read_lines("_pkgdown.yml") %>% 
  stringr::str_subset("^   -") %>% 
  stringr::str_replace("-", "") %>% 
  stringr::str_trim()
  
  expect_equal(sort(pkg),  sort(man))
  left_join(tibble(x = pkg), tibble(x = man)) %>% filter(is.na(x))
  left_join(tibble(x = man), tibble(x = pkg)) %>% filter(is.na(x))
  
  
  
  
  
  man[duplicated(man)]
  
  pkg[duplicated(pkg)]  # xxxcont.
  # problem is that dasymnorm and dasymexp are confounded in html taggged documentation. One of them is right and the other one is wrong.
  
  # With randomRow, I don't know yet.
  
  
  
  
  
  expect_true(purrr::is_empty(setdiff(man, pkg)))
  expect_true(purrr::is_empty(setdiff(pkg, man)))
})
