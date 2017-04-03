# Source: "http://ctfs.si.edu/CTFSAdmin/temp/CTFSRFunctions.txt"

library(readr)

path <- "./data-raw/CTFSRFunctions.txt"
funs_ctfs_rc <- read_tsv(path)$Function
funs_ctfs_fg <- stringr::str_replace(dir("./man"), "\\.Rd", "")

setdiff(funs_ctfs_rc, funs_ctfs_fg)
# "slope.intercept.frompts" was removed intentionally because it exists in the 
# source code but not in the .rda object. "tojulian" seems fine except that it
# lacks a closing "}". This may be solved once I solve warnings, so I won't
# explicitely work on this right now.

setdiff(funs_ctfs_fg, funs_ctfs_rc)
# ctfs is the package documentation. The reason why other functions are missing
# from _rc may likely be because the source code and the table of functions are
# not up to date.
