library(readr)
library(dplyr)
library(purrr)
library(stringr)


read_file("./data-raw/tst.R") %>% 
  str_replace_all(
    pattern = regex(
      "@examples.*(\'dgammaMinusdexp\')",
      multiline = TRUE,
      dotall = TRUE
      ), 
    replacement = "\\1"
    )
    
    
    
    
    
    pattern = fixed("#'\r\n#'\r\n#'\r\n#' @examples\r\n#' \\dontrun{\r\n#'\r\n#'\r\n#'\r\n#' browser() \r\n\r\n\r\n#'\r\n#'\r\n#'\r\n#'\r\n#'\r\n#'\r\n'dgammaMinusdexp'\r\n"),
    replacement = "#\'\r\n\'dgammaMinusdexp\'\r\n"
    ) %>% 
  write_file("./data-raw/tst_to.R")
  


  map(

  

