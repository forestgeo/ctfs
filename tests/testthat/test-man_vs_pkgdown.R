context("Functions in man/ and _pkgdown.yml are the same")


test_that("Functions in man/ are all in _pkgdown.yml and viceversa", {
  
  # Avoid documenting testing internal functions, because they are in man but
  # not indexed
  here <- stringr::str_extract(getwd(), "/[^/]*$")
  
  
  
  # try to skip test when run from check
  if (here != "/forestr") {
    
    
    
    prefix <- ifelse(here == "/forestr" , "./", "../../")
    here_path <- function(here) {paste0(prefix, here)}
    exported_not_indexed <- list_internal_funs(
      raw_strings(path2r = here_path("R/"))
    )
    exported_not_indexed <- exported_not_indexed$fun_internal
    pkg_doc <- "forestr"
    not_applicable <- c(pkg_doc, exported_not_indexed)
    
    man <- dir(here_path("man")) %>% 
    stringr::str_replace("\\.Rd", "") %>% 
    setdiff(not_applicable)
    
    pkg <- readr::read_lines(here_path("_pkgdown.yml")) %>% 
    stringr::str_subset("^   -") %>% 
    stringr::str_replace("-", "") %>% 
    stringr::str_trim()
    
    expect_equal(sort(pkg), sort(man), 
      info = "See showdiff_man_pkg()"
    )
    
    expect_true(purrr::is_empty(setdiff(man, pkg)), 
      info = "See showdiff_man_pkg()"
    )
    expect_true(purrr::is_empty(setdiff(pkg, man)),
          info = "See showdiff_man_pkg()"
    )
    
    expect_true(purrr::is_empty(man[duplicated(man)]),
      info = "See showdiff_man_pkg()"
    )
    expect_true(purrr::is_empty(pkg[duplicated(pkg)]),
      info = "See showdiff_man_pkg()"
    )


      "hey"
  }
})
