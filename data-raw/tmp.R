# Compare forestr and CTFS-CRAN -------------------------------------------

library(dplyr)
fr <- dir("man")
cran <- dir("../CTFS-CRAN/man/")
setdiff(cran, fr)
in_both <- intersect(cran, fr)
in_both <- stringr::str_replace(in_both, ".Rd$", "")

done <- c("abundance", "abundance.spp", "assemble.demography", "ba", 
  "biomass.change", "elev.to.list", "findborderquads", "find.climits", 
  "fgrowth.dbh", "growth", "growth.trim")

setdiff(in_both, done) %>% sort()


library(forestr)
devtools::load_all()
filter_args_by_fun("growth")
















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

