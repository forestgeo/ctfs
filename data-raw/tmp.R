# Compare forestr and CTFS-CRAN -------------------------------------------



# setup -------------------------------------------------------------------

library(dplyr)
library(forestr)
devtools::load_all()

fr <- dir("man")
cran <- dir("../CTFS-CRAN/man/")
setdiff(cran, fr)
in_both <- intersect(cran, fr)
in_both <- stringr::str_replace(in_both, ".Rd$", "")

# record ------------------------------------------------------------------

done <- c("abundance", "abundance.spp", "assemble.demography", "ba", 
  "biomass.change", "elev.to.list", "findborderquads", "find.climits", 
  
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
  "mortality.dbh"
)

setdiff(in_both, done) %>% sort()

# next --------------------------------------------------------------------


args_explore("mortality.eachspp")


elevmat, cran



find_xxxdocparam()





x <- "mortality"
args_of(x)
args_filter_by_fun(x)
args_undoc(x)







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

