library(readr)
library(dplyr)
library(purrr)
library(stringr)


read_file("./data-raw/tst.R") %>% 
  str_replace_all(
    pattern = regex(
      "(\\})\\}.*(\'assemble\\.demography\')",
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\\1\r\n\\2"
    )









    

  # write_file("./data-raw/tst_to.R")
  

