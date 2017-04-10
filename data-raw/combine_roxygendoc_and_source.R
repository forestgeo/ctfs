# In a single file, combine the roxygen documentation generated programatically
# with the original source code and html-tagged documentation.



# Packages ----------------------------------------------------------------

library(ctfs)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)



# Prepare files to be combined ----

# From original (html-tagged) documentation, produce rogygen docum

# Reads from data-raw/CTFSRPackage/ and saves to data-raw/ctfs_doc
source("./data-raw/src2doc_html2roxygen.R")

# Reads from data-raw/CTFSRPackage/ and saves to data-raw/ctfs_src_html/. This
# step basically extracts each file from subfolders out into a single folder

source("./data-raw/src2doc_extract_src_from_subfolders.R")



# Store directories convenietly ----

code_from <- "./data-raw/ctfs_src_html/"
doc_from <- "./data-raw/ctfs_doc/"
to <- "./R/"

# Check if file in one folder lack in the other. "ctfs-package.r" is ok.

diff_code_doc <- setdiff(dir(code_from), dir(doc_from))
if (!purrr::is_empty(diff_code_doc)) {
  warning(
    paste(
      "File(s) in docs folder is not in code folder.", 
      "Difference is:", 
      deparse(diff_code_doc)
    )
  )
}

diff_doc_code <- setdiff(dir(doc_from), dir(code_from))
if (!purrr::is_empty(diff_doc_code)) {
  warning(
    paste(
      "File(s) in docs folder is not in code folder.", 
      "Difference is:", 
      deparse(diff_doc_code)
    )
  )
}

dirs <- tibble(file_nm = intersect(dir(doc_from), dir(code_from))) %>% 
  mutate(
    path_src = paste0(code_from, file_nm),
    path_doc = paste0(doc_from, file_nm),
    path_doc_src = paste0(to, file_nm)
  )



# Combine documentation and source ----

# For each file with multiple functions, paste corresponding documentation and
# source, and save to a single file

combine_doc_src <- function(from_doc, from_src, to) {
  paste0(
    "\n# Roxygen documentation generated programatically --------------------\n",
    read_file(from_doc),
    "\n# Source code and original documentation -----------------------------\n",
    read_file(from_src)
  ) %>% 
    write_file(to)
}

pmap(list(dirs$path_doc, dirs$path_src, dirs$path_doc_src), combine_doc_src)



