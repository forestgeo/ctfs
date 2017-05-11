# test all functions in man are in website

library(tidyverse)
load_all()

raw_strings <- raw_strings()

# xxx cont. 
#   check test
#   add description
#   tidy non standard evaluation





# try 1 -------------------------------------------------------------------

# describe functions

extract_fun_patt <- function(raw_strings) {
  raw_strings %>% 
    stringr::str_extract_all(
      stringr::regex("^\\'[a-z]+.*$", multiline = TRUE)
    ) %>% 
    unlist()
}





pull_description <- function(extracted_patt) {
  fun_patt <- extracted_patt %>% paste0(collapse = "|")
  raw_strings() %>% 
    stringr::str_split(fun_patt) %>% 
    tibble() %>% 
    unnest() %>% 
    set_names("str") %>% 
    mutate(
      str = stringr::str_replace_all(str, stringr::fixed("\r"), ""),
      str = stringr::str_replace_all(str, stringr::fixed("\r"), ""),
      str = stringr::str_replace_all(str, stringr::fixed("\n"), ""),
      str = stringr::str_replace_all(str, stringr::fixed("#"), ""),
      str = stringr::str_replace_all(str, stringr::fixed("'"), ""),
      str = stringr::str_replace_all(str, stringr::fixed("@description"), ""),
      str = stringr::str_trim(str),
      str = stringr::str_trunc(str, 60)
    )
}

tibble_description <- function(pulled) {
  pulled$fun2match <- stringr::str_extract_all(pulled$str, "^[^ ]*")
  pulled %>% unnest
}



# try 2 -------------------------------------------------------------------

fun_comm <- "#' abundance"
fun_replace <- function(fun_comm) {
  fun_comm <- str_replace(fun_comm, "#' ", "")
  replacement <- extract_fun_patt(raw_strings()) %>% 
    pull_description() %>% 
    tibble_description() %>% 
    filter(fun2match == fun_comm)
  paste0("#' ", replacement$str)
}

fun_replace(fun_comm = fun_comm)




# get functions

patterns <- tibble(
  fun = extract_fun_patt(raw_strings())
  ) %>% 
  mutate(fun = str_replace_all(fun, "'", "")) %>% 
  mutate(fun = paste0("#\' ", fun)) %>% 
  .[["fun"]] %>% 
  paste0(collapse = "|")

raw_strings %>% str_replace_all((patterns), paste0("xxx", \\1 ))





# add tag #' fun
# split string by that pattern




# try 3 -------------------------------------------------------------------

# Idea from ?str_replace

library(stringr)
# Use a function for more sophisticated replacement. This example
# replaces colour names with their hex values.
colours <- str_c("\\b", colors(), "\\b", collapse="|")
col2hex <- function(col) {
  rgb <- col2rgb(col)
  rgb(rgb["red", ], rgb["green", ], rgb["blue", ], max = 255)
}

x <- c(
  "Roses are red, violets are blue",
  "My favourite colour is green"
)
str_replace_all(x, colours, col2hex)

col2hex("red")







# try that worked ---------------------------------------------------------

comment_patt <- function() {
  paste0(
    "#' ", 
    extract_fun_patt(raw_strings()) %>% str_replace_all("'", "")
  ) %>% 
    paste0(collapse = "\r|")
}

trunc_description <- function(patt) {
  plus <- paste0(patt, ".{200}")
  title <- raw_strings() %>% 
    str_subset(patt) %>% 
    str_extract(regex(plus, multiline = TRUE, dotall = TRUE)) %>% 
    str_replace_all(patt, "") %>%
    str_replace_all("\r|\n|#|'|@description", "") %>% 
    str_trim() %>% 
    str_trunc(70)
  paste0("#' ", title)
}



# script to change to function?

path2R <- "./data-raw/to_R/"
replaced <- purrr::map(raw_strings(), 
  str_replace_all, 
  pattern = comment_patt, 
  replacement = trunc_description
  ) %>% 
  enframe() %>% 
  mutate(path = paste0(path2R, name, ".R"))

purrr::walk2(replaced$value, replaced$path, write_file)

