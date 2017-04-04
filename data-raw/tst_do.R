library(readr)
library(dplyr)
library(purrr)
library(stringr)


read_file("./data-raw/tst.R") %>% 
  str_replace_all(
    pattern = regex(
      "\'\'dgammaMinusdexp\')", 
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\\1"
    )
    










    

  # write_file("./data-raw/tst_to.R")
  

