library(readr)
library(dplyr)
library(purrr)
library(stringr)


read_file("./data-raw/tst.R") %>% 
  str_replace_all(
    pattern = regex(
      "CTFSplot\\(plot=\'sinharaja,census=3",
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "CTFSplot\\(plot=\'sinharaja', census=3"
    )









    

  # write_file("./data-raw/tst_to.R")
  

