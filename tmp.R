# Test abund.manycensus to see if it works before and after replacing 
# subset by [.


# pkgs --------------------------------------------------------------------

library(testthat)
library(dplyr)
load_all()

# tst data ----------------------------------------------------------------


## S3 method for class 'data'
censdata <- bci::bci12full1
split.data(
  censdata,
  splitcol = "sp",
  keepsplitcol = FALSE,
  allsplit = NULL,
  showOutput = NULL
)

library(stringr)
read_lines("NAMESPACE") %>% 
  str_extract("S3method(.*),(.*)$") %>% 
  as_tibble() %>% 
  rm_na_row() %>% 
  mutate(value = str_replace_all(value, "S3method\\((.*),(.*)\\)$", "\\1.\\2")) 
    
    %>% 
  
  
  str_replace_all(".*exp.*2par.*$", "export(exp.2par)") %>% 
  
  
  
  str_replace_all(".*exp.*2par.*$", "export(exp.2par)") %>% 



