library(readr)
library(dplyr)
library(purrr)
library(stringr)


read_file("./data-raw/tst.R") %>% 
  str_replace_all(
    pattern = regex(
      "(The output is a data\\.frame of direction and slope for the 8 facets, starting with the lower left and moving clockwise).*(\'calc\\.gradient\')", 
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\\1\r\n\\2"
    )









    

  # write_file("./data-raw/tst_to.R")
  

