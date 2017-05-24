

# table params ------------------------------------------------------------

# Requirement

# For each function in R/ extract the parameters



# setup -------------------------------------------------------------------

library(dplyr)
library(stringr)
library(tidyr)
load_all()
raw_strings <- raw_strings()
string <- raw_strings[1]

# funs --------------------------------------------------------------------

# String to grep functions 
functions_splitter <- function() {
  fff <- get_funs(raw_strings())
  fff$fun <- paste0("[[:cntrl:]]'", fff$fun, "'", "[[:cntrl:]]")
  paste0(fff$fun, collapse = "|")
}



subset_fun_with_param <- function(string) {
  splt <- string %>%
    stringr::str_split(functions_splitter()) %>% 
    unlist() 
  splt_funs <- ifelse(length(splt) == 1, splt, splt[1:(length(splt) - 1)])
  
  if (string %>% get_funs() %>% nrow() == 0) {
      tibble(fun = NA_character_, splt_funs) %>% 
        filter(str_detect(splt_funs, "@param"))
  } else {
    cbind(
      string %>% get_funs(),
      tibble(splt_funs)
    ) %>% 
      filter(str_detect(splt_funs, "@param"))
  }
}



table_params <- function(string){
subset_fun_with_param(string) %>% 
  group_by(fun) %>% 
  mutate(params = str_extract_all(splt_funs, "@param [^@]+")) %>% 
  ungroup() %>% 
  select(fun, params) %>% 
  unnest() %>% 
  mutate(
    params = str_replace(params, "@param ", ""),
    definition = str_replace(params, "^[^ ]+ ", ""),
    params = str_extract(params, "^[^ ]+ ")
  ) %>% 
    arrange(params, fun)
}

table_params(raw_strings())  # xxxcont. solve problem. each parameter seems to be repeates for each function















# Extract documentation for each function

# Subset functions with params

# Table function name, param name and definition

# Sort the table by param name, to identify identical or similar paramenters















# Compare forestr and CTFS-CRAN -------------------------------------------

fr <- dir("man")
cran <- dir("../CTFS-CRAN/man/")
setdiff(cran, fr)
in_both <- intersect(cran, fr)
in_both



#' @inheritParams biomass.change
#' param mindbh is the minimum dbh to include in results
#' param dbhunit Either 'mm'or 'cm'
#' param split1,split2 must both be vectors of character variables with exactly
#'   as many elements as there are rows in the tables `census1` and `census2` 
#'   (or both can be NULL), for instance, species names, dbh categories, or 
#'   quadrat numbers.
#' param err.limit See [trim.growth()].
#' param maxgrow See [trim.growth()].






# Procedure to build this package ----

.rs.restartR()
devtools::build()
# devtools::document()

load_all()
library(tidyverse)
write_pkgdown_yml(raw_strings())
.rs.restartR()
devtools::test()
.rs.restartR()
pkgdown::build_site()
.rs.restartR()
devtools::check()
.rs.restartR()
devtools::install()

# To debug difference in documentation between pkgdown and man
# showdiff_man_pkg

