# Make available all functions in the CTFS R package, and some example data.
# This follows installation instructions in
# http://ctfs.si.edu/Public/CTFSRPackage/.

folder <- "./inst/attach/"
path <- paste0(folder, dir(folder))
lapply(path, attach)

# Give users a clear global environment

rm(list = c("folder", "path"))

# Inform users what this file did

message("Files above were attached from .Rprofile")
