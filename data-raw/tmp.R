entitle_function("./data-raw/to_R/")

# Procedure to build this package
# - rebuild package
build()
# - document 
document()
# - write pkgdown.yml
write_pkgdown_yml(raw_strings())
# - test man and pkgdown.yml have the same functions
test()
#     - if not, show
showdiff_man_pkg()
# build site




raw_strings() %>% 
  stringr::str_subset("#' AssignDiag\r") %>% 
  stringr::str_extract("#' AssignDiag\r")

