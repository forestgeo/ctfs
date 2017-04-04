library(readr)
library(dplyr)
library(purrr)
library(stringr)


read_file("./data-raw/tst.R") %>% 
  str_replace_all(
    pattern = regex(
      "(Landau added a section to correct convexity in edge quadrats).*(\'allquadratslopes\')", 
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\\1\r\n\\2"
    )










    

  # write_file("./data-raw/tst_to.R")
  

