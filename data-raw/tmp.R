library(stringr)

formatted_funs <- purrr::map(raw_strings, extract_funs)
formatted_nms <- paste0("\n- title: ", title_folder_files(raw_strings))
arranged <- purrr::map2(formatted_nms, formatted_funs, paste0) %>% 
  paste0(collapse = "\n") %>% 
  str_split("- title: ") %>% 
  tibble() %>% 
  unnest() %>% 
  set_names("str") %>% 
  arrange(str) %>%
  filter(str_detect(str, "[a-zA-Z]")) %>% 
  mutate(str = paste0("- title: ", str)) %>% 

paste0(arranged$str, collapse = "") %>% write_file("tmp.R")







# one list item
raw_strings <- raw_strings()
formatted_funs <- purrr::map(raw_strings, extract_funs) %>% 
  purrr::map(tibble)
formatted_nms <- paste0("\n- title: ", title_folder_files(raw_strings))
purrr::map2(formatted_nms, formatted_funs, paste0) %>% 
  str_split("- title: ")
  
  
  
  paste0(collapse = "\n") %>% 
  str_split("- title: ") %>% 
  tibble() %>% 
  unnest() %>% 
  set_names("str") %>% 
  arrange(str) %>%
  filter(str_detect(str, "[a-zA-Z]")) %>% 
  mutate(str = paste0("- title: ", str)) %>% 

paste0(arranged$str, collapse = "") %>% write_file("tmp.R")























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
