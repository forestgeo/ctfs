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
library(stringr)




# Access to files and functions -----------------------------------------

files_in_R <- paste0("./R/", dir("./R"))
raw_strings <- purrr::map(files_in_R, read_file)
names(raw_strings) <- stringr::str_replace(files_in_R, 
  pattern = "^\\./R/(.*)\\.R$", 
  replacement = "\\1"
)



extract_funs <- function(raw_string){
  extracted <- raw_string %>% 
    stringr::str_extract_all(
      stringr::regex("^\\'[a-z]+.*$", multiline = TRUE)
    ) %>% 
    tibble::tibble() %>% 
    tidyr::unnest() %>% 
    purrr::set_names("funs") %>% 
    dplyr::mutate(
      funs = stringr::str_replace_all(funs, stringr::fixed("'"), ""),
      funs = paste("\n   -", funs)
  )
  header <- paste0(
    "\n  contents:",
    collapse = ""
  )
  funs <- paste0(extracted$funs, collapse = "")
  paste0(header, funs)
}



file_header <- paste0(
  "\nhome:",
  "\n  links:",
  "\n  - text: Learn more",
  "\n    href: http://www.forestgeo.si.edu/",
  "\n",
  "\nreference:"
)

formatted_funs <- purrr::map(raw_strings, extract_funs)
formatted_nms <- paste0("\n- title: ", names(raw_strings))
file_body <- purrr::map2(formatted_nms, formatted_funs, paste0) %>% 
  paste0(collapse = "\n")

paste0(file_header, file_body) %>% write_file("_pkgdown.yml")
