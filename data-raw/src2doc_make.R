# Outline ----

# This files produces roxygen documentation and combines it with its source. The
# steps involved are these:

# WHAT RUNNING THIS FILE DOES
# - 1. produce roxygen documentation;
# - 2. export functions from source;
# - 3. combine roxygen documentation with the corresponding original source;

# TO DO AFTER RUNNING THIS FILE
#   - document to export functions via NAMESPACE and to produce help files;

# - After roxygenizing docs (via data-raw/combine_roxygendoc_and_source.R):
#     + load all,
#     + clean and rebuild,
#     + ensure is dissable "Automatically roxygenize when running:
#         - a. check,
#         - b. build,
#   - Ammend namespace,
#   - check,
#   - build.



# Setup -------------------------------------------------------------------

# Packages

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)



# Create directories

# Temporary; will be removed at the end.
dir.create("./data-raw/ctfs_doc/")
dir.create("./data-raw/ctfs_src_html/")
# Permanent
dir.create("./R/")




# 1. produce roxygen documentation -------------------------------------------

# Read from data-raw/CTFSRPackage/ and save to data-raw/ctfs_doc
source("./data-raw/src2doc_html2roxygen.R")



# 2. export functions from source -----------------------------------------

# Read from data-raw/CTFSRPackage/ and save to data-raw/ctfs_src_html/. This 
# step extracts each file from subfolders out into a single folder, and exports
# the source code so that NAMESPACE processes those exported functions.

source("./data-raw/src2doc_extract_src_from_subfolders.R")



# 3. combine roxygen documentation with the corresponding source ----------

# Store directories convenietly

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



# Paste documentation and (exported) source, and save to a single file

combine_doc_src <- function(from_doc, from_src, to) {
  paste0(
    "\n# Roxygen documentation generated programatically -------------------\n",
    read_file(from_doc),
    "\n# Source code and original documentation ----------------------------\n",
    read_file(from_src)
  ) %>% 
    write_file(to)
}

pmap(list(dirs$path_doc, dirs$path_src, dirs$path_doc_src), combine_doc_src)



# Remove temporary directories

unlink("./data-raw/ctfs_doc", recursive = TRUE)
unlink("./data-raw/ctfs_src_html", recursive = TRUE)

# Copy documentation that needed no process.
file.copy("./data-raw/src2doc_pkg_doc/ctfs-package.R", "./R/ctfs-package.R")

