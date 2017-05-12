entitle_function("./data-raw/to_R/")

# Procedure to build this package
# - rebuild, document
build()
# - write pkgdown.yml
write_pkgdown_yml(raw_strings())
# - test man and pkgdown.yml have the same functions
test()
#     - if not, show
showdiff_man_pkg()
