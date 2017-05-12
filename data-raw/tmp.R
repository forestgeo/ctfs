# Procedure to build this package

.rs.restartR()
devtools::build()
devtools::document()
library(tidyverse)
write_pkgdown_yml(raw_strings())
.rs.restartR()
devtools::test()
pkgdown::build_site()

.rs.restartR()
devtools::check()
devtools::install()

# To debug difference in documentation between pkgdown and man
# showdiff_man_pkg

