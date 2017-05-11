# test all functions in man are in website

library(tidyverse)
load_all()


# xxx cont. 
#   check test
#   add description
#   tidy non standard evaluation






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
