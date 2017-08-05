# Store error messages and maybe give some solutions

# "Can't use matrix or array for column indexing" -------------------------

# transforming tibble in datafreame with as.data.frame(). See Interacting with
# legacy code (https://blog.rstudio.org/2016/03/24/tibble-1-0-0/)


# Here is a case where the problem arises (this code lives inside
# `ctfs::split_data()`):

censdata = bci::bci12full1
splitcol = "sp"
keepsplitcol = FALSE
allsplit = NULL
showOutput = NULL

output=list()

split = censdata[, splitcol]  # Here is the problem! If censdata is a data.frame
                              # then split is a vector, but
                              # if censdata is a tibble, 
                              # then split is another tibble (problem).

# Alternative solutions

# 1. follow advice in https://blog.rstudio.org/2016/03/24/tibble-1-0-0/
split = censdata[, splitcol]
split = as.data.frame(censdata)[, splitcol]
split

# 2. Assume data is a single variable, and directly pull into a vector
split = censdata[, splitcol]
split = censdata[, splitcol][[1]]
split

# 3. As 2, but check first that data is a single variable, then pull into 
split = censdata[, splitcol]
is_single_vector <- dim(split)[2] == 1
if (is_single_vector) {split <- split[[1]]}
split
