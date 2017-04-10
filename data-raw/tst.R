# for each file, read doc, read source, paste source to docdoc_from


file <- "abundance.R"
from_src <- paste0("./data-raw/ctfs_src_html/", file)
from_src <- paste0("./data-raw/ctfs_src_html/", file)


paste0(
  "\n# Roxygen documentation generated programatically --------------------\n",
  read_file(from_doc),
  "\n# Source code and original documentation -----------------------------\n",
  read_file(from_src)
  ) %>% 
  write_file("./data-raw/ctfs_src_doc.R")

