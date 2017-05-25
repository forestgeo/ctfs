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
  "growth.dbh", "growth", "trim.growth", "growth.eachspp", "growth.indiv",
  "gxgy.to.hectindex"
)

setdiff(in_both, done) %>% sort()

# next --------------------------------------------------------------------
x <- "gxgy.to.index"
args_expore <- function(x) {
  list(
    of = args_of(x),
    by_fun = args_filter_by_fun(x),
    undoc = undocumented_args(x)
  )
}
















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

