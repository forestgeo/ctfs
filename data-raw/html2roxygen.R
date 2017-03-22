# Description -------------------------------------------------------------

# tag_read: read all tags in all files

# Packages ----------------------------------------------------------------

library("purrr")
pkgs <- c("dplyr", "readr", "tidyr", "stringr", "tibble")
walk(pkgs, library, character.only = TRUE)

# Directories -------------------------------------------------------------

directory_from   <- "./data-raw/R_from/"
directory_to   <- "./data-raw/R_to/"

from <- map2_chr(directory_from, dir(directory_from), paste0)
to   <- map2_chr(directory_to,   dir(directory_from), paste0)


# Remove end tags ---------------------------------------------------------

extract_tag <- function(path) {
  path %>% 
    read_file %>% 
    stringr::str_extract_all(pattern = "<[a-z]*>") %>% 
    unlist %>% 
    unique
}

base <- directory_from
paths <- paste0(base, dir(base))
tags2rm <- map(paths, read_file) %>% 
  map(extract_tag) %>%
  unlist %>%
  unique %>% 
  tibble %>%
  transmute(tags = stringr::str_replace_all(., "<", "</"))

tags2rm <- str_c(tags2rm[[1]], collapse = "|")

# replace_stuff <- function(path_from, path_to, pattern, replacement, ...) {
replace_stuff <- function(path_from, pattern, replacement) {
  text <- readr::read_file(path_from)
  stringr::str_replace_all(text, pattern, replacement)
}

# Add function name to the end, to document names -------------------------

name_function <- function(string) {
  fun_names <- str_split(string, "Title: ") %>% 
    # unlist %>% 
    map(str_extract, pattern = ".*") %>% 
    unlist
  fun_body <- str_split(string, "Title: ") %>% 
    unlist
  
  tibble(body = fun_body, names = fun_names) %>% 
    transmute(edited = paste0("\n#\' ", body, "\n\'", names, "\'\r\n")) %>% 
    map_chr(paste0, collapse = "\r\n")
  }

# Wrangle -----------------------------------------------------------------

# Remove end tags
map(from, replace_stuff, pattern = tags2rm, replacement = "") %>% 
  # Remove @export
  map(replace_stuff, pattern = "' @export", replacement = "") %>% 
  # Remove function tag
  map(replace_stuff, pattern = "# <function>\\r\\n", replacement = "") %>% 
  # Remove <br>
  map(replace_stuff, pattern = "<br>", replacement = "") %>% 
  # Remove white space after #
  map(replace_stuff, pattern = "(\\r\\n#)[ ]*(\\r\\n)", replacement = "\\1\\2") %>%
  # Remove double spaces and nothing else
  map(replace_stuff, pattern = "^#[ ]*$",replacement = "#") %>% 
  # Tag @description
  map(replace_stuff, pattern = "<description>", replacement = "@description") %>% 
  # Tag @param
  map(replace_stuff, pattern = "<li>", replacement = "@param") %>% 
  # Remove <arguments> and <ul>
  map(replace_stuff, pattern = "# <arguments>\\r\\n|# <ul>\\r\\n", replacement = "") %>% 
  # Remove : after params name
  map(replace_stuff, pattern = "(@param [^:]*):", replacement = "\\1") %>% 
  # Remove source code and tag <source>
  map(replace_stuff, pattern = "^[^#].*", replacement = "") %>% 
  # Replace sample by example
  map(replace_stuff, pattern = "# <sample>", replacement = "# @examples\n# \\\\dontrun\\{") %>% 
  # Replace # by #'
  map(replace_stuff, pattern = "#", replacement = "#'") %>% 
  # Comment uncommented to dissable from roxygen
  map(replace_stuff, pattern = "\n[^#]", replacement = "\n# ") %>% 
  # add title and name to document
  map(replace_stuff, pattern = "#' <name>\\r\\n[#' ](.*)", replacement = "#' Title: \\1") %>% 
  # Add end } of dontrun
  map(replace_stuff, pattern = "(run\\{)?(.*)(\n#'\r\n)", replacement = "\\1\\2\n#' \\}\n") %>% 
  # Remove lines commented without roxygen
  map(replace_stuff, pattern = "\n#[^'] ?(.*)", replacement = "") %>% 
  # Removes source
  map(replace_stuff, pattern = "<source>\r?([^#' Title:]*)", replacement = "<source>\r") %>%
  # Replace } by xxxxx
  map(replace_stuff, pattern = "\n#\' \\}\n", replacement = "\n#\'\r") %>% 
  # Remove <source>
  map(replace_stuff, pattern = "<source>", replacement = "") %>% 
  # Remove  ' in Title
  map(replace_stuff, pattern = "Title: \' ", replacement = "Title: ") %>% 

    # Let devtools document function names by adding functions name to the end
  map(name_function) %>% 
  
  
  # Remove  #' #' 
  map(replace_stuff, pattern = "\'#", replacement = "#") %>%
  map(replace_stuff, pattern = "#\' #\'", replacement = "#\'") %>%
  map(replace_stuff, pattern = "#\' \'", replacement = "#\'") %>%
  
  # Save
  walk2(to, write_file, append = TRUE)
