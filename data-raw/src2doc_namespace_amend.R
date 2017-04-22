# This file ammends NAMESPACE after exporting functions via roxygen. 1. Roxygen
# mistakenly exports some functions as S3 methods.

# Before runing this file, document and export functions via NAMESPACE by 
# clicking Build & Reload. After runing this file, see what to do in
# src2doc_make_step2.R



library(readr)
library(magrittr)
library(stringr)

# Ammend NAMESAPCE after roxygen

read_lines("NAMESPACE") %>% 
  str_replace_all("S3method(.*),(.*)$", "export\\1.\\2") %>% 
  str_replace_all(".*exp.*2par.*$", "export(exp.2par)") %>% 
  write_lines("NAMESPACE")
