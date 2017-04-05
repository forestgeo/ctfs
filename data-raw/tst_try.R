library(readr)
library(magrittr)
library(stringr)
library(purrr)

# base_in <- "./R/"
base_in <- "./data-raw/tst_in/"
base_out <- "./data-raw/tst_out/"
path_in <- map2(base_in, dir(base_in), paste0)
path_out <- map2(base_out, dir(base_in), paste0)

path_in %>% 
  map(read_file) %>% 

  map(str_replace_all, fixed("M*%*E=V"), "M*\\%*E=V") %>% 
  map(str_replace_all, fixed("E=Minv*%*V"), "E=Minv*\\%*V") %>% 
  walk2(path_out, write_file)

# read_file("./data-raw/tst_out/mortality.R")
