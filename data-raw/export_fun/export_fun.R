# This script adds "#' @export" after the tag </name> in all files placed in 
# <directory_from>, and copies the edited files in <directory_to>

# Place all functions in CRTFRPackage out of their folders and in todo/

# Setup ----

library(tidyverse)
library(stringr)

export_fun <- function(path_from, path_to) {
  text <- readr::read_file(path_from)
  text <- stringr::str_replace_all(text, 
    pattern = "# </name>", 
    replacement = "# </name>\n#' @export"
    )
  readr::write_file(text, path_to)
  }



# Do iterative job ----
# Use function defined in setup to do its job for all files in directory_from

directory_from <- "./data-raw/export_fun/todo/"
directory_to   <- "./data-raw/export_fun/done/"

from <- purrr::map2_chr(directory_from, dir(directory_from), paste0)
to   <- purrr::map2_chr(directory_to,   dir(directory_from), paste0) %>% 
  str_replace_all("^(.*)[r]$", "\\1R")  # replace file.r by file.R

purrr::walk2(from, to, export_fun)

# Remove todo/ manually.
# Functions in done/ will export if placed in ./R/
