# Extract function names from each file in R/<FILE>.R and transforms it into 
# an entry to reference for _pgkgdown.yml that looks like this:
# - title: <FILE>
#   contents:
#   - <FUNCTION_1>
#   - <FUNCTION_2>
#   - <FUNCTION_N>



# setup -------------------------------------------------------------------

library(ctfs)
library(tidyverse)




# tibble all files in R/ and functions in each --------------------------

# tibble all file names in R/ and paths

address <- tibble(path = paste0("./R/", dir("./R")))

# do it for 1 file

# tibble file name and the name of each function in that file

# path <- address$path[1]

funs_n_files <- function(path) {
  txt <- readr::read_lines(path) %>% 
    stringr::str_extract("^\\'[a-z]+.*$") %>% 
    stringr::str_replace_all(stringr::fixed("'"), "")
  tbl <- tibble(fun_nm = txt) %>% rm_na_row()
  tbl$fun_nm <- paste("\n   -", tbl$fun_nm)
  paste0(tbl$fun_nm, collapse = "")
}




path_to_file_nm <- function(path){
  stringr::str_replace(path, stringr::fixed("./R/"), "")
}


paste_pkgdown <- function(path) {
  paste0(
    "\n- title: ", path_to_file_nm(path),
    "\n  contents:",
    funs_n_files(path),
    "\n"
  )
}

paste_pkgdown(path)


# iterate over all
address$path %>% 
  purrr::map(paste_pkgdown)









