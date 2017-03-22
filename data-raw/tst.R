library(readr)
library(stringr)
from <- "./data-raw/tsttex.R"
to <- "./data-raw/tsttex_done.R"

string <- read_file(from)

name_function <- function(string) {
  fun_names <- str_split(string, "Title: ") %>% 
    # unlist %>% 
    map(str_extract, pattern = ".*") %>% 
    unlist
  fun_body <- str_split(string, "Title: ") %>% 
    unlist
  
  tibble(body = fun_body, names = fun_names) %>% 
    transmute(edited = paste0("\n#\' ", body, "\n\'", names, "\'\r\n")) %>% 
    map_chr(paste0, collapse = "\r\n")
  }


tb <- tibble(tb = c("Hi", "Mauro"))
tb %>% map_chr(paste0, collapse = "\r\n") %>% write_file(to)









