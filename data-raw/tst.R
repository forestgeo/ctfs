library(readr)
library
from <- "./data-raw/tsttex.R"
to <- "./data-raw/tsttex_done.R"
read_file(from) %>% 
  str_replace_all(
    "\n#\' \\}\r", 
    "\n#\'\r"
    ) %>% 
  write_file(to, append = TRUE)



