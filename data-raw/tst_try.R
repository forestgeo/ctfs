library(readr)
library(stringr)
library(purrr)
library(tibble)
library(dplyr)
library(tidyr)


base_in <- "./data-raw/tst_in/"
base_out <- "./data-raw/tst_out/"
path_in <- map2(base_in, dir(base_in), paste0)
path_out <- map2(base_out, dir(base_in), paste0)

file_dir <- "./data-raw/tst_in/abundance.R"



tibble_doc <- function(file_dir) {
  file_dir %>% 
    readr::read_file() %>% 
    str_replace_all(
      pattern = regex(
        "(.*\n\')([^ ]+)(\'.*)",
        multiline = TRUE,
        dotall = TRUE
      ),
      replacement = "\\1\\2\nxxxxxxxxxxxxxxx\n\\3"
    ) %>% 
    str_split("xxxxxxxxxxxxxxx") %>% 
    unlist() %>% 
    tibble() %>% 
    setNames("doc") %>% 
    # Remove empty rows
    filter(grepl(".*[a-zA-Z]+.*", doc))
}


# Make pattern [^ ]

tibble_fun_nm <- function(file_dir) {
  tibble_doc(file_dir) %>% 
    mutate(
      fun_nm = str_extract_all(
        doc,
        pattern = regex(
          "\n\'[a-zA-Z]+[a-zA-Z]|\\.]*\'", 
          multiline = TRUE,
          dotall = TRUE
        )
      ),
      fun_nm = str_replace(fun_nm, fixed("\n\'"), "")
    ) %>% 
    dplyr::select(2, 1)
}


tibble_doc("./data-raw/tst_in/utilitiesCTFS.R")
tibble_fun_nm("./data-raw/tst_in/utilitiesCTFS.R") %>% View


# code_from <- "./data-raw/ctfs_src_html/"
# doc_from <- "./data-raw/ctfs_doc/"

map_df(path_in, tibble_fun_nm)[[1]]
map_df(path_in =  dirs$path_doc, tibble_fun_nm)[[1]]
