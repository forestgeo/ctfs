# Utility functions grouped by main functionality
# Although not intended for users, some functions are exported to make it easy
# to call them from other functions. But they are removed from the index with
# `@keywords internal`




# rm_na_row ---------------------------------------------------------------

#' Detect rows in a data frame full of NA (accross columns).
#'
#' @param .data A non empty matrix or data frame
#' @return A logical vector of length equal to number of rows in data.
#' @export
#' @keywords internal
#' @examples
#' library(tibble)
#' df <- tribble(
#' ~x,  ~y,    ~z,
#'  1, "a", FALSE,
#' NA,  NA,    NA,
#' 5,  "b",  TRUE,
#' NA,  NA,    NA,
#' NA, "c",    NA
#' )
#' is_na_row(df)
is_na_row <- function(.data) {
  assertive::assert_is_non_empty(.data)
  assertive::assert_any_are_true(
    c(is.matrix(.data), is.data.frame(.data))
  )
  is_na_vector <- function(x) all(is.na(x))
  apply(.data, 1, is_na_vector)
}

#' Remove rows from data frame or matrix full of NA.
#' 
#' @param .data A data frame or matrix.
#' @return Output and `.data`. have the same type.
#' @export
#' @keywords internal
#' @examples
#' library(tibble)
#' df <- tibble::tribble(
#' ~x,  ~y,    ~z,
#'  1, "a", FALSE,
#' NA,  NA,    NA,
#' 5,  "b",  TRUE,
#' NA,  NA,    NA,
#' NA, "c",    NA
#' )
#' rm_na_row(df)
rm_na_row <- function(.data) {
  assertive::assert_is_non_empty(.data)
  assertive::assert_any_are_true(
    c(is.matrix(.data), is.data.frame(.data))
  )
  .data[!is_na_row(.data), ]
}






# write_pkgdown_yml -------------------------------------------------------

# To build the reference section of the package website, write a _pkgdown.yml 
# referencing the folder and file where each function comes from the original 
# source  code of the CTFSRPackage. Functions are sorded first by folder, then
# by file and function.
#
# The head of the file looks like this:
#
# home:
#   links:
#   - text: Learn more
#     href: http://www.forestgeo.si.edu/
# 
# reference:
# - title: abundance; abundance
#   contents: 
#    - abund.manycensus
#    - abundance
#    - ...



# The field title has a reference of the fol
# Access files and functions
raw_strings <- function(path2r = "./R/") {
  path2r
  files_in_R <- paste0(path2r, dir(path2r))
  raw_strings <- purrr::map(files_in_R, readr::read_file)
  names(raw_strings) <- stringr::str_replace(
    files_in_R, 
    pattern = "^\\./R/(.*)\\.R$", 
    replacement = "\\1"
  )
  raw_strings
}

# From a list of raw strings, extract functions in each and format for pkgdown
get_funs <- function(raw_strings){
  raw_strings %>% 
    stringr::str_extract_all(
      stringr::regex("^\\'[a-zA-Z]+.*$", multiline = TRUE)
    ) %>% 
    tibble::tibble() %>% 
    tidyr::unnest() %>% 
    purrr::set_names("fun") %>% 
    dplyr::mutate(
      fun = stringr::str_replace_all(fun, stringr::fixed("'"), "")
    )
}

# Sort by folder, file and functions
tibble_folder_file_fun <- function(raw_strings) {
  folder_files <- readr::read_csv("./data-raw/folder_files.csv")
  file_functions <- purrr::map(raw_strings, get_funs) %>% 
    tibble::enframe() %>% 
    tidyr::unnest() %>%
    dplyr::rename(file = name)
  dplyr::right_join(folder_files, file_functions) %>% 
    dplyr::arrange(folder, file, fun)
}



# Write body of the _pkgdown file
tibble_fff <- function(tibble_folder_file_fun) {
  tibble_folder_file_fun %>% 
  dplyr::group_by(folder, file) %>% 
  dplyr::mutate(
    fun = paste0("\n   - ", fun),
    fun = paste0(fun, collapse = "")
    ) %>% 
  unique()
}
format_pkgdown <- function(tibble_fff) {
  tibble_fff %>% 
  dplyr::transmute(
    reference = paste0("\n- title: ", folder, "; ", file, "\n  contents: "),
    reference = paste0(reference, fun, collapse = "\n")
  ) %>% 
  .[["reference"]] %>% 
  paste0(collapse = "\n")
}
site_ref_body <- function(raw_strings) {
  tibble_folder_file_fun(raw_strings) %>% 
    tibble_fff() %>% 
    format_pkgdown()
}



# Write header of the pkgdown file
site_ref_head <- function() {
    paste0(
    "home:",
    "\n  links:",
    "\n  - text: Learn more",
    "\n    href: http://www.forestgeo.si.edu/",
    "\n",
    "\nreference:"
  )
}

# Combine all of the above in one single step
pkgdown_doc_nms <- function(raw_strings) {
  paste0(site_ref_head(), site_ref_body(raw_strings)) %>% 
    readr::write_file("_pkgdown.yml")
}



# List internal functions

list_internal_funs <- function(raw_strings) {
raw_strings %>% 
  stringr::str_subset("@keywords internal") %>%
  stringr::str_split("\n\r") %>% 
  tibble::tibble() %>% 
  tidyr::unnest() %>% 
  purrr::set_names("fun_internal") %>%
  dplyr::filter(grepl("@keywords internal", fun_internal)) %>%
  dplyr::mutate(
    fun_internal = stringr::str_extract_all(
      fun_internal, "[A-z0-9_.]+ <- function"
    )
  ) %>% 
  tidyr::unnest() %>% 
  dplyr::mutate(
    fun_internal = stringr::str_replace_all(
      fun_internal, " <- function", ""
    )
  ) %>% 
    dplyr::filter(fun_internal != "list_internal_funs")
}

showdiff_man_pkg <- function(current_dir = "./") {
  path2r <- paste0(current_dir, "R/")
  path2man <- paste0(current_dir, "man")
  path2pkdownyml <- paste0(current_dir, "_pkgdown.yml")

  exported_not_indexed <- list_internal_funs(raw_strings(path2r))
  exported_not_indexed <- exported_not_indexed$fun_internal
  pkg_doc <- "forestr"
  not_applicable <- c(pkg_doc, exported_not_indexed)
  
  man <- dir(path2man) %>% 
  stringr::str_replace("\\.Rd", "") %>% 
  setdiff(not_applicable)
  
  pkg <- readr::read_lines(path2pkdownyml) %>% 
  stringr::str_subset("^   -") %>% 
  stringr::str_replace("-", "") %>% 
  stringr::str_trim()
  
  list(
    pkg_man = setdiff(sort(pkg),  sort(man)),
    man_pkg = setdiff(sort(man),  sort(pkg))
    )
}



file_of_fun <- function(fun) {
strings <- raw_strings() %>% 
  tibble::enframe() %>% 
  tidyr::unnest()
strings[grepl(paste0(fun, " <- function"), strings$value), ]$name
}

tibble_no_folder <- function(fun) {
  tibble::tibble(
    folder = ".",
    file = file_of_fun(fun),
    fun = fun
  )
}



format_diff_man_pkg <- function() {
  showdiff_man_pkg()$man_pkg %>%
    purrr::map_df(tibble_no_folder) %>%
    tibble_fff() %>% 
    format_pkgdown()
}



pkgdown_diff_man_pkg <- function() {
  paste(
    readr::read_file("_pkgdown.yml"), 
    format_diff_man_pkg(), sep = "\n"
  ) %>% 
    write_file("_pkgdown.yml")
}



# test

# Test that functions in man/ and _pkgdown.yml are the same")

test_man_vs_pkg <- function() {
  # Test that functions in man/ are all in _pkgdown.yml and viceversa
  
  # Avoid documenting testing internal functions, because they are in man but
  # not indexed
  here <- stringr::str_extract(getwd(), "/[^/]*$")
  prefix <- ifelse(here == "/forestr" , "./", "../../")
  here_path <- function(here) {paste0(prefix, here)}
  exported_not_indexed <- list_internal_funs(
    raw_strings(path2r = here_path("R/"))
  )
  exported_not_indexed <- exported_not_indexed$fun_internal
  pkg_doc <- "forestr"
  not_applicable <- c(pkg_doc, exported_not_indexed)
  
  man <- dir(here_path("man")) %>% 
  stringr::str_replace("\\.Rd", "") %>% 
  setdiff(not_applicable)
  
  pkg <- readr::read_lines(here_path("_pkgdown.yml")) %>% 
  stringr::str_subset("^   -") %>% 
  stringr::str_replace("-", "") %>% 
  stringr::str_trim()
  
  testthat::expect_equal(sort(pkg), sort(man), 
    info = "See showdiff_man_pkg()"
  )
  
  testthat::expect_true(purrr::is_empty(setdiff(man, pkg)), 
    info = "See showdiff_man_pkg()"
  )
  testthat::expect_true(purrr::is_empty(setdiff(pkg, man)),
        info = "See showdiff_man_pkg()"
  )
  
  testthat::expect_true(purrr::is_empty(man[duplicated(man)]),
    info = "See showdiff_man_pkg()"
  )
  testthat::expect_true(purrr::is_empty(pkg[duplicated(pkg)]),
    info = "See showdiff_man_pkg()"
  )
}



# Run and test

write_pkgdown_yml <- function(raw_strings) {
  # write _pkgdown.yml from functions documented as names with 'name'
  pkgdown_doc_nms(raw_strings)
  # To _pkgdown.yml, add functions present  in man but not in pkg, which are 
  # new functions, documented properly as: name <- function().
  pkgdown_diff_man_pkg()
  # test is not outside testthat because OK test fails in devtools::check()
  test_man_vs_pkg()
}







# table params ------------------------------------------------------------

# The goal is to know
# - what parameters are documented,
# - which parameters are documented in more than one function,
# - which parameters are named differently but their definition indicates they
# should be named the same.
# The taks is therefore to table documented parameters along with the
# functions where they are documented.
# The scope is the functions that come from ctfs; not new functions ones.



# String to match functions used to split strings in by functions
functions_splitter <- function() {
  fff <- get_funs(raw_strings())
  fff$fun <- paste0("[[:cntrl:]]'", fff$fun, "'", "[[:cntrl:]]")
  paste0(fff$fun, collapse = "|")
}

# From functions in R/, subset those with documented parameters
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
        dplyr::filter(stringr::str_detect(splt_funs, "@param"))  # xxx no need?
  } else {
    cbind(
      string %>% get_funs(),
      tibble::tibble(splt_funs)
    ) %>% 
      dplyr::filter(stringr::str_detect(splt_funs, "@param"))
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
        params = stringr::str_extract(params, "^[^ ]+ "),
        params = stringr::str_trim(params)
      )
  }
}

args_unstick <- function(string) {unlist(stringr::str_split(string, ","))}

# Tables all documented parameters
table_params_all <- function(string = raw_strings(), update = FALSE) {
  if(!update) {
    message("To update ./data-raw/params_table.csv, re-run with update = TRUE")
  }
  params_table <- purrr::map_df(string, table_params) %>% 
    dplyr::arrange(params, fun) %>% 
    rm_na_row() %>% 
    dplyr::filter(!is.na(fun)) %>% 
    dplyr::mutate(params = params %>% purrr::map(args_unstick)) %>% 
    tidyr::unnest()
    
  if(update) {
    devtools::use_data(params_table, overwrite = TRUE, internal = TRUE)
    readr::write_csv(params_table, "./data-raw/params_table.csv")
    message("Writting (or rewritting) to './data-raw/params_table.csv'"
    )
    }
  params_table
}

args_filter_by_fun <- function(funname) {
  par <- names(formals(funname))
  params_table %>%  # must be in ".R/sysdata.rda"
    dplyr::filter(params %in% par) %>% 
    dplyr::select(fun, params, definition)
    
}






# Find undocumented arguments ------------------------------------------------

# Find arguments that are undocumented, considering that some arguments are
# formatted as argument1,argument2,argument3

# Helpers

# e.g. args_of("growth.eachspp")
args_of <- function(x) {names(formals(x))}

args_undoc_one <- function(fun) {
  valid_functions <- unique(get_funs(raw_strings())$fun)
  stopifnot(fun %in% valid_functions)
  
  not_explicitely_documented <- setdiff(
    names(formals(fun)),
    args_filter_by_fun(fun)$params
  )
  setdiff(
    not_explicitely_documented,
    unique(params_table$params)
  )
}

args_multi_documented <- function(args) {
  patt <- "@param [A-z0-9_\\.]+,[A-z0-9_\\.]+"
  stringr::str_extract_all(raw_strings(), patt) %>% 
    tibble() %>% 
    tidyr::unnest() %>% 
    setNames("fun") %>% 
    mutate(fun = stringr::str_replace(fun, "@param ", "")) %>% 
    dplyr::filter(stringr::str_detect(fun, args))
}



# Implementation (wrapper)

args_undoc <- function(fun) {
  undoc <- args_undoc_one(fun)
  if (length(undoc) == 0) {
    message("All arguments are documented somewhere.")
  } else {
    multi_documented_args <- args_multi_documented(undoc)$fun %>% 
      stringr::str_split(",") %>% 
      unlist()
    setdiff(undoc, multi_documented_args)
  }
}

# Wrap multiple functions to explore arguments documentation
args_explore <- function(x) {
  of <- tibble::tibble(arguments = args_of(x))
  by_fun <- args_filter_by_fun(x) %>% 
    mutate(definition = stringr::str_trunc(definition, 40))
  undoc <- if (length(args_undoc_one(x) == 0)) {
    args_undoc_one(x)
    } else {
      args_undoc(x)
    }
  
  list(args_of = of, args_by_fun = by_fun, args_undoc = undoc)
  # list(of = of, by_fun = by_fun, undoc = undoc)
}



# Find the pattern xxxdocparam to documente parameters (then ctr + shift + f).
find_xxxdocparam <- function() {
  purrr::flatten_chr(
    stringr::str_extract_all(raw_strings(), "#\' @param [^ ]+ xxxdocparam")
  )
}





# end ---------------------------------------------------------------------

