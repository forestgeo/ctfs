# Plan devleopment --------------------------------------------------------

# Start with code + html-tagged-docs and roxygen documentation in separate files

# Code. Copy source files from a folder with subfolders to a single folder
source("./data-raw/src2doc_src_to_dir.R")

# Roxygen documentation. From sourse files, wrangle to roxygenize documentation
source("./data-raw/src2doc_html2roxygen.R")




# Preliminary ----

# In data-raw place source and doc

# Docs. Roxygenize into ./data-raw/ctfs_docs/ (instead of into ./R/)

# Source. Copy source into ./data-raw/ctfs_source/ (instead of into ./R/)



# Outline ----
#   0. store the name of the file with many functions in a tibble's variable
#   1. store the roxygen docs of each function in a file in a tibble's variable
#   2. store the source of each function in a file in a tibble's variable
#   3. paste the doc and source of each function
#   4. for each file name, combine rows into a single text string
#   5. save each file with its corresponding docs and source of each function



# Work ----

#   0. store the name of the file with many functions in a tibble's variable

# tibble paths to docs files and source files, and ensure filenames match



#   1. store the roxygen docs of each function in a file in a tibble's variable

# Read each docs and source file, split by function name and tibble. Now, tibble
# should have a column with file name, function (fun) name, fun source, fun doc.


# xxxcont.


#   2. store the source of each function in a file in a tibble's variable
#   3. paste the doc and source of each function
#   4. for each file name, combine rows into a single text string
#   5. save each file with its corresponding docs and source of each function
