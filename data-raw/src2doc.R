# Plan devleopment --------------------------------------------------------

# Start with code + html-tagged-docs and roxygen documentation in separate files

# Code: Copy source files from a folder with subfolders to a single folder
# source("./data-raw/src2doc_src_to_dir.R")  # WARNING: DISSABLED ONCE RUN

# Roxygen documentation: From sourse files, wrangle to roxygenize documentation
# source("./data-raw/src2doc_html2roxygen.R")  # WARNING: DISSABLED ONCE RUN



# Outline ----
#   0. store the name of the file with many functions in a tibble's variable
#   1. store the roxygen docs of each function in a file in a tibble's variable
#   2. store the source of each function in a file in a tibble's variable
#   3. paste the doc and source of each function
#   4. for each file name, combine rows into a single text string
#   5. save each file with its corresponding docs and source of each function



# Packages ----------------------------------------------------------------

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)




# Work --------------------------------------------------------------------

# 0. tibble file paths and names. Each file has multiple functions. ----

# Check if file in one folder lack in the other. "ctfs-package.r" is ok.

code_from <- "./data-raw/ctfs_src_html/"
doc_from <- "./data-raw/ctfs_doc/"

diff_code_doc <- setdiff(dir(code_from), dir(doc_from))
if (!purrr::is_empty(diff_code_doc)) {
  warning("File(s) in docs folder is not in code folder")
  diff_code_doc
  }

diff_doc_code <- setdiff(dir(doc_from), dir(code_from))
if (!purrr::is_empty(diff_doc_code)) {
  warning("File(s) in docs folder is not in code folder")
  diff_doc_code
  }

dirs <- tibble(file_nm = intersect(dir(doc_from), dir(code_from))) %>% 
  mutate(
    path_src = paste0(code_from, file_nm),
    path_doc = paste0(doc_from, file_nm)
    )


# xxxcont. ----

# 1. store the roxygen docs of each function in a file in a tibble's variable ----

# Read each docs and source file, split by function name and tibble. Now, tibble
# should have a column with file name, function (fun) name, fun source, fun doc.



#   2. store the source of each function in a file in a tibble's variable
#   3. paste the doc and source of each function
#   4. for each file name, combine rows into a single text string
#   5. save each file with its corresponding docs and source of each function
