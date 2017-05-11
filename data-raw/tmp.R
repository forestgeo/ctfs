# test all functions in man are in website

library(tidyverse)
load_all()

#################


# xxxcont. integrate this in utils. Insert funs in chain to continue formatting.
write_pkgdown_yml(raw_strings())

write_pkgdown_yml(
  fun = showdiff_man_pkg()$man_pkg,
  raw_strings = raw_strings()
)









###############

  stringr::str_extract_all(
      stringr::regex(
        pat,
        multiline = TRUE,
        dotall = TRUE
      )
  )




%>% 
    tibble::tibble() %>% 
    tidyr::unnest() %>% 
    purrr::set_names("fun") %>% 
    dplyr::mutate(
      fun = stringr::str_replace_all(fun, stringr::fixed("'"), "")
    )







not_applicable <- c("forestr", "rm_na_row", "wsgdata_dummy", "is_na_row")

man <- dir("./man") %>% 
  stringr::str_replace("\\.Rd", "") 




pkg <- read_lines("_pkgdown.yml") %>% 
  stringr::str_subset("^   -") %>% 
  stringr::str_replace("-", "") %>% 
  stringr::str_trim()

setdiff(man, pkg)







setdiff(pkg, man)
intersect(man, pkg)










# describe functions



pull_description <- function(raw_strings) {
  fun_patt <- raw_strings %>% 
    stringr::str_extract_all(
      stringr::regex("^\\'[a-z]+.*$", multiline = TRUE)
    ) %>% 
    unlist() %>% 
    paste0(collapse = "|")
  
  raw_strings %>% 
    stringr::str_split(fun_patt) %>% 
    tibble() %>% 
    unnest() %>% 
    set_names("str") %>% 
    mutate(
      str = stringr::str_replace_all(str, stringr::fixed("\r"), ""),
      str = stringr::str_replace_all(str, stringr::fixed("\r"), ""),
      str = stringr::str_replace_all(str, stringr::fixed("\n"), ""),
      str = stringr::str_replace_all(str, stringr::fixed("#"), ""),
      str = stringr::str_replace_all(str, stringr::fixed("'"), ""),
      str = stringr::str_replace_all(str, stringr::fixed("@description"), ""),
      str = stringr::str_trim(str),
      str = stringr::str_trunc(str, 60)
    )
}

tibble_description <- function(raw_strings) {
  pulled <- pull_description(raw_strings)
  pulled$fun2match <- stringr::str_extract_all(pulled$str, "^[^ ]*")
  pulled %>% unnest
}


tibble_description(raw_strings)
