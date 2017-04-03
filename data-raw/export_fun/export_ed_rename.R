# Rename each file to start with its original folder name

library(tidyverse)
library(stringr)

# Table files and folders

path_pkg <- "./inst/CTFSRPackage/"
pkg <- map2_chr(path_pkg, dir(path_pkg), paste0) %>% 
  map(dir) %>% 
  set_names(dir(path_pkg)) %>% 
  tibble() %>% 
  set_names("file") %>% 
  mutate(folder = dir(path_pkg)) %>%
  unnest()

# Add new names and save
pkg <- pkg %>% 
  mutate(
    file = str_replace_all(file, "^(.*)[r]$", "\\1R"),
    newname = paste0(folder, "_", file)
    ) %>% 
  write_csv("./data-raw/export_fun/pkg_ctfs.csv")

base <- "./data-raw/export_fun/done/"
pkg <- pkg %>% 
  mutate(
    file = paste0(base, file), 
    newname = paste0(base, newname)
    )

purrr::walk2(pkg[["file"]], pkg[["newname"]], file.rename)

# All done, move files from done/ to ./R/
