

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
  only_uselesss_functions <- length(splt) == 1
  splt_funs <- if (only_uselesss_functions) {
    splt
  } else {
    splt[1:(length(splt) - 1)]
  }
  
  if (string %>% get_funs() %>% nrow == 0) {
      tibble::tibble(fun = NA_character_, splt_funs) %>% 
        dplyr::filter(str_detect(splt_funs, "@param"))
  } else {
    cbind(
      string %>% get_funs(),
      tibble::tibble(splt_funs)
    ) %>% 
      dplyr::filter(str_detect(splt_funs, "@param"))
  }
}

table_params <- function(string){
  with_params <- subset_fun_with_param(string)
  if (nrow(with_params) == 0) {
    tibble::tibble(fun = NA_character_, definition = NA_character_)
  } else {
    with_params %>%
      dplyr::group_by(fun, splt_funs) %>%
      mutate(params = stringr::str_extract_all(splt_funs, "@param [^@]+")) %>%
      dplyr::ungroup() %>%
      dplyr::select(fun, params) %>%
      tidyr::unnest() %>%
      dplyr::mutate(
        params = stringr::str_replace(params, "@param ", ""),
        definition = stringr::str_replace(params, "^[^ ]+ ", ""),
        params = stringr::str_extract(params, "^[^ ]+ ")
      )
  }
}



# implement ----
# Tables all documented parameters
table_params_all <- function(string = raw_strings()) {
  purrr::map_df(string, table_params) %>% arrange(params, fun) %>% rm_na_row()
}

table_params_all() %>% View()




# Test cases ----

# has content
table_params(raw_strings()[4])
# all na
table_params(raw_strings()[5])

for (i in seq_along(raw_strings())) {
  print(ncol(table_params(raw_strings()[[i]])))
}

raw_strings()[4] %>% table_params()
raw_strings()[10] %>% table_params()
















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

