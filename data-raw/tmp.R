
# Compare forestr and CTFS-CRAN -------------------------------------------

fr <- dir("man")
cran <- dir("../CTFS-CRAN/man/")
setdiff(cran, fr)
in_both <- intersect(cran, fr)
in_both



#' @inheritParams biomass.change
#' param mindbh is the minimum dbh to include in results
#' param dbhunit Either 'mm'or 'cm'
#' param split1,split2 must both be vectors of character variables with exactly
#'   as many elements as there are rows in the tables `census1` and `census2` 
#'   (or both can be NULL), for instance, species names, dbh categories, or 
#'   quadrat numbers.
#' param err.limit See [trim.growth()].
#' param maxgrow See [trim.growth()].











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

