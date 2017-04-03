library(readr)
library(dplyr)
library(purrr)
library(stringr)



replace_params <- function(string) {
  args_asis <- string %>% 
    str_extract_all(
      pattern = regex(
        "<arguments>.*</arguments>",
        multiline = TRUE,
        dotall = TRUE
        )
      ) %>% unlist
  args_edited <- args_asis %>% 
    str_replace_all(
      pattern = regex("<li>", multiline = TRUE, dotall = TRUE),
      replacement = "@param"
      )
  string %>% 
    str_replace_all(pattern = fixed(args_asis), replacement = args_edited)
}

map("./data-raw/tst.R", read_file) %>% 
  map(replace_params) %>% 
  walk2("./data-raw/tst_to.R", write_file, append = TRUE)

