library(readr)
library(stringr)
library(purrr)
library(tibble)
library(dplyr)
library(tidyr)


base_in <- "./data-raw/tst_in/"
base_out <- "./data-raw/tst_out/"
path_in <- map2(base_in, dir(base_in), paste0)
path_out <- map2(base_out, dir(base_in), paste0)




tibble_doc <- function(file_dir) {
  file_dir %>% 
    readr::read_file() %>% 
    str_replace_all(
      pattern = regex("(\n\')([^ ]+)(\'\n)", multiline = F, dotall = F),
      replacement = "\\1\\2\\3\nxxxxx\n"
      ) %>% 
    str_split("xxxxx") %>% 
    unlist() %>% 
    tibble() %>% 
    setNames("doc") %>% 
    # Remove empty rows
    filter(grepl(".*[a-zA-Z]+.*", doc))
}

tibble_fun_nm <- function(file_dir) {
  src_nm <- tibble_doc(file_dir) %>% 
    mutate(
      fun_nm = str_extract_all(
        doc, 
        pattern = regex(".*(\n\')([^ ]+)(\'\n).*", multiline = F, dotall = F)
        ),
      fun_nm = str_replace_all(fun_nm, fixed("#'\n'"), ""),
      fun_nm = str_replace_all(fun_nm, fixed("\n'"), ""),
      fun_nm = str_replace_all(fun_nm, fixed("'\n"), "")
      )
  dplyr::select(src_nm, 2, 1)
}






file_dir <- "./data-raw/ctfs_src_html/abundance.R"

tibble_src <- function(file_dir) {
  file_dir %>% 
    readr::read_file() %>% 
    str_replace_all(
      pattern = regex(
        "(<name>.*<\\\\/name>).*(<source>.*<\\\\/source>)", 
        multiline = T, 
        dotall = T
        ),
      replacement = "\\1\nxxxxxx\n\\2"
      ) %>% 
    str_split("xxxxx") %>% 
    unlist() %>% 
    tibble() %>% 
    setNames("doc") %>% 
    # Remove empty rows
    filter(grepl(".*[a-zA-Z]+.*", doc))
}












map(path_in, tibble_fun_nm)

