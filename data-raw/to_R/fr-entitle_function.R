# Write a title for each function using 70 characters after documentation start.
# This routine operates on the functions from the original source code of the 
# CTFSRPackage, which title was produced programatically (as #' function_name).
# It shoud be necessary only once and never again.

extract_fun_patt <- function(raw_strings) {
  raw_strings %>% 
    stringr::str_extract_all(
      stringr::regex("^\\'[a-z]+.*$", multiline = TRUE)
    ) %>% 
    unlist()
}

comment_patt <- function() {
  paste0(
    "#' ", 
    extract_fun_patt(raw_strings()) %>% stringr::str_replace_all("'", "")
  ) %>% 
    paste0(collapse = "\r|")
}

trunc_description <- function(patt) {
  plus <- paste0(patt, ".{200}")
  title <- raw_strings() %>% 
    stringr::str_subset(patt) %>% 
    stringr::str_extract(
      stringr::regex(plus, multiline = TRUE, dotall = TRUE)
    ) %>% 
    stringr::str_replace_all(patt, "") %>%
    stringr::str_replace_all("\r|\n|#|'|@description", "") %>% 
    stringr::str_trim() %>% 
    stringr::str_trunc(70)
  paste0("#' ", title)
}

entitle_function <- function(path2R){
  comment_patt <- comment_patt()
  replaced <- purrr::map(raw_strings(), 
    stringr::str_replace_all, 
    pattern = comment_patt, 
    replacement = trunc_description
    ) %>% 
    tibble::enframe() %>% 
    dplyr::mutate(path = paste0(path2R, name, ".R"))
  if (!dir.exists(path2R)) {dir.create(path2R)}
  purrr::walk2(replaced$value, replaced$path, write_file)
}
