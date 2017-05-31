# Compare forestr and CTFS-CRAN -------------------------------------------



# setup -------------------------------------------------------------------

library(dplyr)
library(forestr)
devtools::load_all()

lower_strip_rd <- function(string) {
  tolower(
    stringr::str_replace(string, ".Rd$|.rd$", "")
  )
}
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














# next --------------------------------------------------------------------

table_params_all(update = T)

# commit

devtools::load_all()

# Explore what arguments are most duplicated. Work on those to maximize benefit
# from effort unit

params_table %>% 
  count(params) %>% 
  left_join(params_table) %>% 
  arrange(desc(n), params, fun) %>% 
  View














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

