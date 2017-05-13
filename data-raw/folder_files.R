# GOAL
# The ultimate goal is to help users find functions by topic. Stuart and Gabriel
# believe that topics are well represented by the name of the folder and file in
# which each function lives in the source  code of  the original CTFSRPackage.
# Thus, the specific  goal of this file is  to get the name of the folders and
# files in them, to latter associate folders with functions via merging  by file
# name.
#
# MECHANICS
# From the source of the CTFSRPackage (available online, but here using a local 
# copy), table names of each folder and the names of each file in each folder. 
# The goal is to associate the name of each function to the name  of the file
# and folder from which the function comes from.



library(tidyverse)
library(stringr)



path2folders <- "../ctfs/data-raw/CTFSRPackage"
folders <- dir(path2folders)
path2files <- paste(path2folders, folders, sep = "/")

files_list <- purrr::map(path2files, dir)
names(files_list) <- dir(path2folders)
enframe(files_list) %>% 
  purrr::set_names(c("folder", "file")) %>% 
  tidyr::unnest() %>%
  mutate(file = str_replace(file, "\\.r", "")) %>% 
  write_csv("./data-raw/folder_files.csv")
