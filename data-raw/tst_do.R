library(readr)
library(dplyr)
library(purrr)
library(stringr)


read_file("./data-raw/tst.R") %>% 
  str_replace_all(
    pattern = regex(
      "(\n\'distance\')", 
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\n#\' \\}\r\\1"
    )










    

  # write_file("./data-raw/tst_to.R")
  

