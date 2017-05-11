# Utility functions grouped by main functionality
# Although not intended for users, some functions are exported to make it easy
# to call them from other functions. But they are removed from the index with
# `@keywords internal`




# rm_na_row ---------------------------------------------------------------

#' Detect rows in a data frame full of NA (accross columns)
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

#' Remove rows from data frame or matrix full of NA
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
tibble_fff <- function(raw_strings) {
  tibble_folder_file_fun(raw_strings) %>% 
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
  tibble_fff(raw_strings) %>% format_pkgdown()
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
write_pkgdown_yml <- function(raw_strings) {
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
  )
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

# showdiff_man_pkg()$man_pkg %>% 
#   # c("wsgdata_dummy", "abundance") %>% 
#   purrr::map_df(tibble_no_folder) %>% 
#   group_by(folder, file) %>% 
#   format_pkgdown() %>% 
#   write_file("tmp.yml")

# end ---------------------------------------------------------------------

