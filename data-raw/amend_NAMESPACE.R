# 1. Documenting with roxygen (this will mistakenly export some functions as
#    S3 methods);
# 2. Run this code to ammend NAMESPACE and make  functions be functions, not
#    S3 methods;
# 4. Check
# 5. Build

library(readr)
library(magrittr)
library(stringr)

# Ammend NAMESAPCE after roxygen

read_lines("NAMESPACE") %>% 
  str_replace_all("S3method(.*),(.*)$", "export\\1.\\2") %>% 
  str_replace_all(".*exp.*2par.*$", "export(exp.2par)") %>% 
  write_lines("NAMESPACE")
