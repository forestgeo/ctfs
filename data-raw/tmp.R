
# setup -------------------------------------------------------------------

library(tidyverse)
library(stringr)

devtools::load_all()

lower_strip_rd <- function(string) {
  tolower(
    stringr::str_replace(string, ".Rd$|.rd$", "")
  )
}



# Which arguments are documented and which aren't? ------------------------

# The goal is to reduce the number of undocumented arguments ASAP; this can be
# done by identifying which arguments are most commonly needed and which are or
# aren't documented. The fastest strategy should be to start documenting those 
# that are formals in the higher number of function but are documented the
# smaller number of functions.

library(tidyverse)
devtools::load_all()

args_in_man <- table_args_in_man() %>% 
  count(params) %>% 
  rename(n_man = n) %>% 
  right_join(table_args_in_man()) %>% 
  arrange(n_man, params, fun) %>% 
  left_join(params_table)

# Exclude items that are not functions
not_a_function <- c("forestr", "MONTHNAMES")
args_in_man <- args_in_man %>% filter(! fun %in% not_a_function)

args_formals <- args_in_man %>% 
  mutate(formals = purrr::map(fun, args_of)) %>% 
  unnest()









args_formals %>% 
  count(formals) %>% 
  rename(n_frml = n) %>% 
  rename(params = formals) %>% 
  right_join(args_formals) %>% 
  arrange(desc(n_frml), params, n_man, fun) %>% 
  View()

# xxxcont. now go off and document!

















# Compare params in forestr versus CTFS'CRAN ------------------------------

fr <- lower_strip_rd(dir("man"))
cran <- lower_strip_rd(dir("../CTFS-CRAN/man/"))
setdiff(cran, fr)
in_both <- intersect(cran, fr)



# record ------------------------------------------------------------------

# Same names

done <- tolower(c(
  "abundance", 
  "abundance.spp", 
  "assemble.demography", 
  "ba", 
  "biomass.change", 
  "elev.to.list", 
  "findborderquads", 
  "find.climits", 
  "growth", 
  "growth.dbh", 
  "growth.eachspp", 
  "growth.indiv",
  "trim.growth", 
  "gxgy.to.hectindex", 
  "gxgy.to.index", 
  "index.to.rowcol",
  "gxgy.to.rowcol",
  "findborderquads",
  "index.to.gxgy",
  "map",
  "maptopo",
  "mortality",
  "mortality.calculation",
  "mortality.dbh",
  "mortality.eachspp",
  "readelevdata",
  "recruitment",
  "recruitment.eachspp",
  "rowcol.to.index",
  "tojulian",
  "AGB.ind"
))
done <- tolower(done)
setdiff(in_both, done)  # differ only in case
# Because they seem useless, I intentionally decided not to work on these
# functions:
excluded_useless <- "TextToRdata"



# Similar names 

# Find functions in forestr that are similar to functions in CTFS-CRAN
similar <- tibble::tribble(
  ~fr, ~cran,
  "pop.change", "abundance.change",
  "pop.change.dbh", "abundance.change.dbh",
  "abundanceperquad", "abundance.quad",
  "biomass.change", "biomass",
  "selectrandquad", "select.randquad",
  "split_data", "splitdata"
)

# What functions remain to explore?
remain <- setdiff(cran, c(done, similar$cran)) %>% sort()
remain
length(remain)

# Find duplicated params --------------------------------------------------

table_params_all(update = T)

# commit

devtools::load_all()

# Explore what arguments are most duplicated. Work on those to maximize benefit
# from effort unit

params_table %>% 
  count(params) %>% 
  left_join(params_table) %>% 
  arrange(desc(n), params, fun) %>% 
  # select(params, n) %>% 
  filter(n > 1) %>%
  unique() %>% 
  summarise(sum(n))



library(readr)
library(stringr)
read_lines("string.R") %>% 
  str_replace("^.*([0-9])$", "\\1") %>% 
  as.numeric() %>% sum()


























# Procedure to build this package ----

.rs.restartR()
devtools::build()
# devtools::document()

load_all()
library(tidyverse)
write_pkgdown_yml(raw_strings())
.rs.restartR()
devtools::test()
.rs.restartR()
pkgdown::build_site()
.rs.restartR()
devtools::check()
.rs.restartR()
devtools::install()

# To debug difference in documentation between pkgdown and man
# showdiff_man_pkg

