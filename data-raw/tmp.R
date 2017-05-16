
# Compare forestr and CTFS-CRAN -------------------------------------------

fr <- dir("man")
cran <- dir("../CTFS-CRAN/man/")
in_both <- intersect(cran, fr)

setdiff(cran, fr)
















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

