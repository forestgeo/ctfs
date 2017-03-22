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

replace_stuff <- function(path_from, path_to, pattern, replacement, ...) {
  text <- readr::read_file(path_from)
  text <- stringr::str_replace_all(text, pattern, replacement)
  readr::write_file(text, path_to, append = TRUE)
}

# Apply -------------------------------------------------------------------

# Remove end tags
purrr::walk2(from, to, replace_stuff, pattern = tags2rm, replacement = "")

# Remove export
# purrr::walk2(xxx, xxx, replace_stuff, pattern = "' @export", replacement = "")

# # Remove <function>
# to2 <- purrr::map2_chr("./data-raw/done2/", dir(directory_from), paste0)
# purrr::walk2(to1, to2, replace_stuff, pattern = "<function>", replacement = "")






