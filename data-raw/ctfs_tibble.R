# Goal: store ctfs compressed, into a tibble with columns "name" and "function"

# # Started from loading CTFSRPackage.Rdata into global environment.
# library(dplyr)

# # Get ls() in a tibble
#
# tibble::enframe(
#   setNames(
#     purrr::map(as.list(ls()), get), ls()
#     )
#   ) %>%
#   readr::write_rds("./ctfs.rds", compress = "gz")

# CTFSRPackage.Rdata is 595 KB and ctfs.rds is 93.8 KB, so 6 x smaller.


# Load
# read file with ctfs names and functions
ctfs <- readr::read_rds("C:/Users/dora/Dropbox/git_repos/ctfs/inst/ctfs.rds")
# filter through the functions you want
library(dplyr)
funs <- c("abundance", "map")
filtered <- filter(ctfs, name %in% funs)
# convert to named list
filtered <- setNames(filtered[[2]], filtered[[1]])
list2env(filtered, environment())

