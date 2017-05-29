# Compare forestr and CTFS-CRAN -------------------------------------------



# setup -------------------------------------------------------------------

library(dplyr)
library(forestr)
devtools::load_all()

fr <- dir("man")
cran <- dir("../CTFS-CRAN/man/")
setdiff(cran, fr)
in_both <- intersect(cran, fr)

strip_rd <- function(string) {stringr::str_replace(string, ".Rd$", "")}
in_both <- strip_rd(in_both)


# record ------------------------------------------------------------------

done <- c(
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
  "tojulian"
)



# Find functions in forestr that are similar to functions in CTFS-CRAN
similar <- tibble::tribble(
  ~fr, ~cran,
  "pop.change", "abundance.change"
)

# What functions remain to explore?
setdiff(strip_rd(cran), c(done, similar$fr)) %>% sort()


# next --------------------------------------------------------------------


args_explore("fromjulian")

params_table %>% filter(grepl(params, "maxgrow"))



x <- "mortality"
args_of(x)
args_filter_by_fun(x)
args_undoc(x)


tp <- table_params_all()


string <- "addlegend,legpos,legsize"

args_unstick <- function(string) {unlist(stringr::str_split(string, ","))}

tp %>% 
  # group_by(param) %>% 
  mutate(params = params %>% purrr::map(args_unstick)) %>% 
  tidyr::unnest() %>% 
  filter(fun == "map") %>% View



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

