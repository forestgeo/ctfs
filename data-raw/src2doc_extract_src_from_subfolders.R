# Copy source files (code and html tagged comments) from a folder with
# subfolders to a single folder

library(readr)
library(magrittr)
library(stringr)
library(purrr)

# Where the files (nested in subfolders) live?
base_in <- "./data-raw/CTFSRPackage/"
# Where the files (loose; all in one folder) should go?
base_out <- "./data-raw/ctfs_src_html/"

src_folders <- pmap(list(base_in, dir(base_in), "/"), paste0)
src_files <- map(src_folders, dir)
src_path <- map2(src_folders, src_files, paste0) %>% unlist

dir_out <- map2(base_out, src_files, paste0) %>% unlist
dir_out <- str_replace(dir_out, "\\.r$", "\\.R")

file.copy(from = src_path, to = dir_out)
