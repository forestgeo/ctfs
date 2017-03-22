# Description -------------------------------------------------------------

# tag_read: read all tags in all files

# Packages ----------------------------------------------------------------

library("purrr")
pkgs <- c("dplyr", "readr", "tidyr", "stringr", "tibble")
walk(pkgs, library, character.only = TRUE)

# Directories -------------------------------------------------------------

directory_from <- "./R/"
directory_to   <- "./data-raw/done/"

from <- purrr::map2_chr(directory_from, dir(directory_from), paste0)
to   <- purrr::map2_chr(directory_to,   dir(directory_from), paste0)


# Remove end tags ---------------------------------------------------------

extract_tag <- function(path) {
  path %>% 
    read_file %>% 
    stringr::str_extract_all(pattern = "<[a-z]*>") %>% 
    unlist %>% 
    unique
}

base <- "./R/"
paths <- paste0(base, dir(base))
tags2rm <- map(paths, read_file) %>% 
  map(extract_tag) %>%
  unlist %>%
  unique %>% 
  tibble %>%
  transmute(tags = stringr::str_replace_all(., "<", "</"))

tags2rm <- str_c(tags2rm[[1]], collapse = "|")






# replace_stuff <- function(path_from, path_to, pattern, replacement, ...) {
#   text <- readr::read_file(path_from)
#   text <- stringr::str_replace_all(text, pattern, replacement)
#   readr::write_file(text, path_to, append = TRUE)
# }

# replace_stuff <- function(path_from, path_to, pattern, replacement, ...) {
replace_stuff <- function(path_from, pattern, replacement) {
  text <- readr::read_file(path_from)
  stringr::str_replace_all(text, pattern, replacement)
}


# Wrangle -----------------------------------------------------------------

# Remove end tags
lst <- purrr::map(from, replace_stuff, pattern = tags2rm, replacement = "") %>% 
  # Remove @export
  purrr::map(replace_stuff, pattern = "' @export", replacement = "") %>% 
  # Remove function tag
  purrr::map(replace_stuff, pattern = "# <function>\\r\\n", 
    replacement = "") %>% 
  # Remove <br>
  purrr::map(replace_stuff, pattern = "<br>", replacement = "") %>% 
  # Remove white space after #
  purrr::map(replace_stuff, pattern = "(\\r\\n#)[ ]*(\\r\\n)",
    replacement = "\\1\\2") %>%
  # Remove double spaces and nothing else
  purrr::map(replace_stuff, pattern = "^#[ ]*$",replacement = "#") %>% 
  # Tag @description
  purrr::map(replace_stuff, pattern = "<description>", 
    replacement = "@description") %>% 
  # Tag @param
  purrr::map(replace_stuff, pattern = "<li>", replacement = "@param") %>% 
  # Remove <arguments> and <ul>
  purrr::map(replace_stuff, pattern = "# <arguments>\\r\\n|# <ul>\\r\\n", 
    replacement = "") %>% 
  # Remove : after params name
  purrr::map(replace_stuff, pattern = "(@param [^:]*):", 
    replacement = "\\1") %>% 
  # Remove source code and tag <source>
  purrr::map(replace_stuff, pattern = "^[^#].*", replacement = "") %>% 
  # Replace sample by example
  purrr::map(replace_stuff, pattern = "# <sample>", 
    replacement = "# @examples") %>% 
  # Replace # by #'
  purrr::map(replace_stuff, pattern = "#", replacement = "#'") %>% 
  # Comment uncommented to dissable from roxygen
  purrr::map(replace_stuff, pattern = "\n[^#]", replacement = "\n# ") %>% 
  # add title and name to document
  purrr::map(replace_stuff, pattern = "#' <name>\\r\\n[#' ](.*)", 
    replacement = "#' Title: \\1")

  #   # add title and name to document
  # purrr::map(replace_stuff, pattern = "[#' Title: ](.*)", 
  #   replacement = "#' Title: \\1") %>% 


# Save
walk2(lst, to, write_file, append = TRUE)





name_fun <- function(path_from, extpatt, pattern, replacement) {
  text <- readr::read_file(path_from)
  stringr::str_extract(text, extpatt)
  stringr::str_replace_all(text, pattern, replacement)
}






