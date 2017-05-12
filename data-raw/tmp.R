# test all functions in man are in website

library(tidyverse)
library(stringr)
load_all()


# Write a title for each function using 70 characters after documentation start

extract_fun_patt <- function(raw_strings) {
  raw_strings %>% 
    stringr::str_extract_all(
      stringr::regex("^\\'[a-z]+.*$", multiline = TRUE)
    ) %>% 
    unlist()
}

comment_patt <- function() {
  paste0(
    "#' ", 
    extract_fun_patt(raw_strings()) %>% str_replace_all("'", "")
  ) %>% 
    paste0(collapse = "\r|")
}

trunc_description <- function(patt) {
  plus <- paste0(patt, ".{200}")
  title <- raw_strings() %>% 
    str_subset(patt) %>% 
    str_extract(regex(plus, multiline = TRUE, dotall = TRUE)) %>% 
    str_replace_all(patt, "") %>%
    str_replace_all("\r|\n|#|'|@description", "") %>% 
    str_trim() %>% 
    str_trunc(70)
  paste0("#' ", title)
}

entitle_function <- function(path2R){
  comment_patt <- comment_patt()
  replaced <- purrr::map(raw_strings(), 
    str_replace_all, 
    pattern = comment_patt, 
    replacement = trunc_description
    ) %>% 
    enframe() %>% 
    mutate(path = paste0(path2R, name, ".R"))
  if (!dir.exists(path2R)) {dir.create(path2R)}
  purrr::walk2(replaced$value, replaced$path, write_file)
}
entitle_function("./data-raw/to_R/")
