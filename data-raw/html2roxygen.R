# Description -------------------------------------------------------------

# tag_read: read all tags in all files



# Prepare -----------------------------------------------------------------

# 1. Download source of CTFSRPackage from
# http://ctfs.si.edu/Public/CTFSRPackage/index.php/web/topics and unzip
# 2. Place CTFSRPackage folder in //data-raw/



# Packages ----------------------------------------------------------------

library("purrr")
pkgs <- c("dplyr", "readr", "tidyr", "stringr", "tibble")
walk(pkgs, library, character.only = TRUE)



# Directories -------------------------------------------------------------

from_folder <- "./data-raw/CTFSRPackage/"
from_subfolder <- map2_chr(from_folder, dir(from_folder), paste0)
to_folder <- "./R"

paths <- tibble(
    from = from_subfolder,
    to = to_folder,
    file = map(from_subfolder, dir)
  ) %>% 
  unnest() %>% 
  transmute(
    from = paste0(from, "/", file),
    to = paste0(to, "/", file)
  ) %>% 
  mutate(to = str_replace(to, "\\.r", "\\.R"))






# FUNCTIONS ===============================================================

# Replace <li> by @param, but only within <arguments> ... </arguments> tags
replace_params <- function(string) {
  args_asis <- string %>% 
    str_extract_all(
      pattern = regex(
        "<arguments>.*</arguments>",
        multiline = TRUE,
        dotall = TRUE
        )
      ) %>% unlist
  args_edited <- args_asis %>% 
    str_replace_all(
      pattern = regex("<li>", multiline = TRUE, dotall = TRUE),
      replacement = "@param"
      )
  string %>% 
    # In string, replace the edited chunk
    str_replace_all(pattern = fixed(args_asis), replacement = args_edited) %>% 
    # Make an Rmarkdown list with the remaining <li>
    str_replace_all("<li>", " * ")
}



# Remove end tags (</SOME_END_TAG>)

extract_tag <- function(path) {
  path %>% 
    read_file %>% 
    stringr::str_extract_all(pattern = "<[a-z]*>") %>% 
    unlist %>% 
    unique
}

tags2rm <- map(paths$from, read_file) %>% 
  map(extract_tag) %>%
  unlist %>%
  unique %>% 
  tibble %>%
  transmute(tags = stringr::str_replace_all(., "<", "</"))

tags2rm <- str_c(tags2rm[[1]], collapse = "|")



# General purpose replace stuff function

replace_stuff <- function(path_from, pattern, replacement) {
  text <- readr::read_file(path_from)
  stringr::str_replace_all(text, pattern, replacement)
}



# Add function name to the end, to document names

name_function <- function(string) {
  fun_names <- str_split(string, "Title: ") %>% 
    # unlist %>% 
    map(str_extract, pattern = ".*") %>% 
    unlist
  fun_body <- str_split(string, "Title: ") %>% 
    unlist
  
  tibble(body = fun_body, names = fun_names) %>% 
    transmute(edited = paste0("\n#\' ", body, "\n\'", names, "\'\r\n")) %>% 
    map_chr(paste0, collapse = "\r\n")
  }



# Wrangle -----------------------------------------------------------------

# Read each file
map(paths$from, read_file) %>% 
  # Replace <li> by @param tags within <arguments>...</arguments> tags
  map(replace_params) %>% 
  # Remove end tags
  map(replace_stuff, pattern = tags2rm, replacement = "") %>% 
  # Remove function tag
  map(replace_stuff, pattern = "# <function>\\r\\n", replacement = "") %>% 
  # Remove <br>
  map(replace_stuff, pattern = "<br>", replacement = "") %>% 
  # Remove white space after #
  map(replace_stuff, pattern = "(\\r\\n#)[ ]*(\\r\\n)", replacement = "\\1\\2") %>%
  # Remove double spaces and nothing else
  map(replace_stuff, pattern = "^#[ ]*$",replacement = "#") %>% 
  # Tag @description
  map(replace_stuff, pattern = "<description>", replacement = "@description") %>% 
  # Remove <arguments> and <ul>
  map(replace_stuff, pattern = "# <arguments>\\r\\n|# <ul>\\r\\n", replacement = "") %>% 
  # Remove : after params name
  map(replace_stuff, pattern = "(@param [^:]*):", replacement = "\\1") %>% 
  # Remove source code and tag <source>
  map(replace_stuff, pattern = "^[^#].*", replacement = "") %>% 
  # Replace sample by example
  map(replace_stuff, pattern = "# <sample>", replacement = "# @examples\n# \\\\dontrun\\{") %>% 
  # Replace # by #'
  map(replace_stuff, pattern = "#", replacement = "#'") %>% 
  # Comment uncommented to dissable from roxygen
  map(replace_stuff, pattern = "\n[^#]", replacement = "\n# ") %>% 
  # add title and name to document
  map(replace_stuff, pattern = "#' <name>\\r\\n[#' ](.*)", replacement = "#' Title: \\1") %>% 
  # Add end } of dontrun
  map(replace_stuff, pattern = "(run\\{)?(.*)(\n#'\r\n)", replacement = "\\1\\2\n#' \\}\n") %>% 
  # Remove lines commented without roxygen
  map(replace_stuff, pattern = "\n#[^'] ?(.*)", replacement = "") %>% 
  # Removes source
  map(replace_stuff, pattern = "<source>\r?([^#' Title:]*)", replacement = "<source>\r") %>%
  # Replace } by xxxxx
  map(replace_stuff, pattern = "\n#\' \\}\n", replacement = "\n#\'\r") %>% 
  # Remove <source>
  map(replace_stuff, pattern = "<source>", replacement = "") %>% 
  # Remove  ' in Title
  map(replace_stuff, pattern = "Title: \' ", replacement = "Title: ") %>% 

  # Let devtools document function names by adding functions name to the end
  map(name_function) %>% 
  
  
  # Details of function titles and names
  map(replace_stuff, pattern = "\'#", replacement = "#") %>%
  map(replace_stuff, pattern = "#\' #\'", replacement = "#\'") %>%
  map(replace_stuff, pattern = "#\' \'", replacement = "#\'") %>%
  map(replace_stuff, pattern = "\'#\'", replacement = "\'") %>%
  map(replace_stuff, pattern = "\' ", replacement = "\'") %>%
  map(replace_stuff, pattern = "(#\')([a-zA-Z])", replacement = "\\1 \\2") %>%
  map(replace_stuff, pattern = "\'\'", replacement = "#\'") %>%
  
  # Give space after @param
  map(replace_stuff, pattern = "(@param)([^ ])", replacement = "\\1 \\2") %>%


  
  # Close brackets if examples are empty
  map(replace_stuff, pattern = "(run\\{)?([\r\n#\']+)(#\')(\\})", replacement = "\\1\\4") %>%
  # Remove empty examples
  map(replace_stuff, pattern = "(@examples)?(.*)run\\{\\}", replacement = "xxxxx") %>%
  # Remove empty examples    "(#\'\r\)(\n#\'@examples?(.*)xxxxx"
  map(replace_stuff, pattern = "@examples\r?(\nxxxxx)", replacement = "") %>%

  # Specific issues following checks
  map(replace_stuff, pattern = " MINIMUM_SD=0.00", replacement = "") %>%
  map(replace_stuff, pattern = "(maxgrow = 75\\))\\}", replacement = "\\1\r\n#\' \\}") %>%
  
  map(replace_stuff, pattern = "@param( full=extract\\.growthdata)", replacement = "\\1") %>%
  map(replace_stuff, pattern = "@param( trimmed=extract\\.growthdata)", replacement = "\\1") %>%
  map(replace_stuff, pattern = "(distances of rseq)", replacement = "\\1\r\n#\' \\}\r") %>%

  
# Solve missmatched braces ------------------------------------------------
  map(
    replace_stuff, 
    pattern = "each distance interval, for graphing purposes.\\}", 
    replacement = "each distance interval, for graphing purposes\\."
  ) %>% 
  map(
    replace_stuff, 
    pattern = "##\' count of individuals in each block", 
    replacement = "#\' \\}"
  ) %>% 
  map(
    replace_stuff, 
    pattern = "the lower confidence limit for the null hypothesis\\. \\}", 
    replacement = "the lower confidence limit for the null hypothesis\\."
    ) %>% 
  map(
    replace_stuff,
    pattern = regex(
      "@examples.*(\\\n\'dgammaMinusdexp\')", 
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\\1"
    ) %>% 
  map(
    replace_stuff,
    pattern = regex(
      "(dontrun.*)( y=y/sum\\(y\\))",
      multiline = TRUE,
      dotall = TRUE
      ), 
    replacement = "\\1\\2\\}"
    ) %>% 
  map(
    replace_stuff,
    pattern = regex(
      "(#\' dpois.max.*)@examples.*(\'dpois.max\')",
      multiline = TRUE,
      dotall = TRUE
      ),
    replacement = "\\1\\\r\\\n\\2"
    ) %>%
  map(
    replace_stuff,
    pattern = regex(
      "\\}.*(\\\n\'pts\\.to\\.interceptslope\')", 
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\\1"
    ) %>% 
  map(
    replace_stuff,
    pattern = regex(
      "\\}.*\'inside\\.rect\\'", 
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\r\n'inside.rect'"
    ) %>% 
  map(
    replace_stuff,
    pattern = regex(
      "(llike=rep\\(0,length\\(modeled\\)\\))", 
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\\1\\}"
    ) %>% 
  map(
    replace_stuff,
    pattern = regex(
      "(\n\'distance\')", 
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\n#\' \\}\r\\1"
    ) %>% 
  map(
    replace_stuff,
    pattern = regex(
      "(Landau added a section to correct convexity in edge quadrats).*(\'allquadratslopes\')", 
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\\1\r\n\\2"
    ) %>% 
  map(
    replace_stuff,
    pattern = regex(
      "(This runs equations 1-3 of Seibert & McGlynn).*(\'calc\\.directionslope\')", 
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\\1\r\n\\2"
    ) %>% 
  map(
    replace_stuff,
    pattern = regex(
      "(The output is a data\\.frame of direction and slope for the 8 facets, starting with the lower left and moving clockwise).*(\'calc\\.gradient\')", 
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\\1\r\n\\2"
    ) %>% 
  map(
    replace_stuff,
    pattern = regex(
      "(This is solely for use by solve\\.topo\\. It finds all points linked via a sighting to a given point\\.).*(\'getTopoLinks\')", 
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\\1\r\n\\2"
    ) %>% 
  map(
    replace_stuff,
    pattern = regex(
      "(whichtest is the index of the parameter to test\\. If NULL, zero, or NA, it simply returns allparam\\.).*(\'arrangeParam\\.llike\')", 
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\\1\r\n\\2"
    ) %>% 
  map(
    replace_stuff,
    pattern = regex(
      "(Updates the regression slope \\(used in regression.Bayes\\)\\.).*(\'Gibbs\\.regslope\')", 
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\\1\r\n\\2"
    ) %>% 
  map(
    replace_stuff,
    pattern = regex(
      "(6\\) the new parameter tested \\(whether accepted or not\\)).*(\'metrop1step\')", 
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\\1\r\n\\2"
    ) %>% 
  map(
    replace_stuff,
    pattern = regex(
      "(is no step-size thus no adjustment\\.).*(\'metrop1step\\.discrete\')",
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\\1\r\n\\2"
    ) %>% 
  map(
    replace_stuff,
    pattern = regex(
      "(For testing mcmc1step. No longer used\\.).*(\'testmcmcfunc\')",
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\\1\r\n\\2"
    ) %>% 
  map(
    replace_stuff,
    pattern = regex(
      "(A form of grep returning logical instead of indices \\(numbers\\)\\.).*(\'logical\\.grep\')",
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\\1\r\n\\2"
    ) %>% 
  map(
    replace_stuff,
    pattern = regex(
      "CTFSplot\\(plot=\'sinharaja,census=3",
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "CTFSplot\\(plot=\'sinharaja', census=3"
    ) %>% 
  map(
    replace_stuff,
    pattern = regex(
      "(\\})\\}.*(\'assemble\\.demography\')",
      multiline = TRUE, 
      dotall = TRUE
      ),
    replacement = "\\1\r\n\\2"
    ) %>% 

  



    
  
  
  
  


  # Let tags breath
  map(replace_stuff, pattern = "\n#\'@param", replacement = "\n#\' @param") %>%
  map(replace_stuff, pattern = "\n#\'@description", replacement = "\n#\' @description") %>%
  map(replace_stuff, pattern = "\n#\'@examples", replacement = "\n#\' @examples") %>%
  map(replace_stuff, pattern = "\n#\'\\\\dontrun", replacement = "\n#\' \\\\dontrun") %>%
  map(replace_stuff, pattern = "<supplemental>", replacement = "@section Supplemental") %>%
  map(replace_stuff, pattern = "</supplemental>", replacement = "") %>%
  map(replace_stuff, pattern = "<i>", replacement = "") %>%

  # Coment a bunch of uncommented example lines
  map(replace_stuff, pattern = "(#\' )(If a split)", replacement = "\\1# \\2") %>%
  map(replace_stuff, pattern = "(#\' )(Otherwise start)", replacement = "\\1# \\2") %>%
  map(replace_stuff, pattern = "(#\' )(A quick test)", replacement = "\\1# \\2") %>%
  map(replace_stuff, pattern = "(#\' )(All the species)", replacement = "\\1# \\2") %>%
  map(replace_stuff, pattern = "(#\' )(calculate K and the)", replacement = "\\1# \\2") %>%
  map(replace_stuff, pattern = "(#\' )(distances of rseq)", replacement = "\\1# \\2") %>%
  
  map(replace_stuff, pattern = "(#\')(@param)", replacement = "\\1 \\2") %>%
  map(replace_stuff, pattern = "(\n#\')\\}\r", replacement = "\\1\r") %>%
  
  map(replace_stuff, pattern = "#\' Author: ", replacement = "#\' @author ") %>%
  
  # Remove function slope.intercept.frompts because it is not included in
  # CTFSRPackage
  map(
    str_replace,
    pattern = stringr::regex(
      "slope\\.intercept\\.frompts.*\'slope\\.intercept\\.frompts\'",
      multiline = TRUE,
      dotall = TRUE
      ),
    replacement = "") %>%
  
  # Save wrangled files
  walk2(paths$to, write_file, append = TRUE)



# Add files to .R/ that do not need wrangling
path_notwrangle <- "./data-raw/R_from_notwrangle/"
from_notwrangle <- map2(path_notwrangle, dir(path_notwrangle), paste0)
to_notwrangle <- map2("./R/", dir(path_notwrangle), paste0)

map(from_notwrangle, read_file) %>% 
  walk2(to_notwrangle, write_file, append = TRUE)
