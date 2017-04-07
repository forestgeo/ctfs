library(readr)
library(magrittr)
library(stringr)
library(purrr)

base_in <- "./data-raw/tst_in/"
base_out <- "./data-raw/tst_out/"
path_in <- map2(base_in, dir(base_in), paste0)
path_out <- map2(base_out, dir(base_in), paste0)

path_in %>% 
  map(read_file) %>% 
  # add cutting point
  str_replace_all(
    regex("(.*'\r\n[^a-zA-Z]*)(\n#\' [a-zA-Z])", multiline = T, dotall = T),
    "\\1xxxxxxxxxxxxxxxxxx\\2"
  ) %>% 
  str_split("xxxxxxxxxxxxxxxxxx")
    

