library(readr)
library(dplyr)
library(purrr)
library(stringr)


read_file("./data-raw/tst.R") %>% 
  str_replace_all(
    pattern = stringr::regex(
      "(parameters for every step.*)<\\\\ul>",
      multiline = TRUE,
      dotall = TRUE
      ),
    replacement = "\\1"
    )

