
# setup -------------------------------------------------------------------

library(tidyverse)
library(stringr)

devtools::load_all()



# Which arguments are documented and which aren't? ------------------------

# The goal is to reduce the number of undocumented arguments ASAP; this can be
# done by identifying which arguments are most commonly needed and which are or
# aren't documented. The fastest strategy should be to start documenting those 
# that are formals in the higher number of function but are documented the
# smaller number of functions.

# I am interested in the difference between arguments needed minus arguments
# documented




# For now, let's try to inherit, so let's skip arguments:
#    - x and y because they are too generic,
#    - arguments I already worked on,
#    - arguments that are all NA, so there is no reference of what they mean.
avoid <- c(
  "x", "y",  # too generic; definition changes from fun to fun
  "...",  # too variable
  "plotdim",  # done
  "gridsize",  # done
  "mindbh",  # done
  "debunit",  # done
  "export",  # pendent; I'm unsure if the template
  "xrange"  # skip, too variable
  
)
x <- args_count_formals_man() %>% 
  filter(!params %in% avoid) %>%
  group_by(params) %>% 
  mutate(some_but_not_all_is_na = some_but_not_all_is_na(man_n)) %>% 
  filter(some_but_not_all_is_na) %>% print_all()
print(x, n = x$frml_n[[1]])



args_count_formals_man() %>% 
  filter(params == "add")













# work on plotdim
args_pull_definitions("plotdim")[[2]]
# cool, there is a single definition

fun_family("allquadratslopes") %>% filter(params == "plotdim")
# There is no other function too closetly related

# 
































# Arguments that are documented directly as @params in functions documentation
args_count_in_funs <- function() {
    params_table %>% 
    dplyr::count(params, sort = TRUE) %>% 
    dplyr::left_join(params_table) %>% 
    dplyr::arrange(desc(n), params, fun) %>% 
    dplyr::rename(funs_n = n) %>% 
    dplyr::select(params, funs_n, fun) %>% 
    dplyr::group_by(params) %>% 
    dplyr::mutate(funs_doc = paste(fun, collapse = ", ")) %>%
    dplyr::ungroup() %>% 
    dplyr::select(-fun) %>% 
    unique()
}
args_count_in_funs()

# Args documented in templates

args_count_in_templates <- function() {
  args_in_templates() %>% 
    dplyr::count(params, sort = TRUE) %>% 
    dplyr::left_join(args_in_templates()) %>% 
    dplyr::arrange(n, params) %>% 
    dplyr::rename(tmplt_n = n) %>% 
    dplyr::select(params, tmplt_n, template) %>% 
    unique()
}






args_count_in_templates() %>% right_join(args_count_in_funs())



args_count_in_man <- args_in_man() %>% 
  count(params, sort = TRUE) %>% 
  left_join(args_in_man()) %>% 
  arrange(n, params, fun) %>% 
  rename(n_in_man = n)
args_count_in_man


args_filter_everywhere("x")


















lower_strip_rd <- function(string) {
  tolower(
    stringr::str_replace(string, ".Rd$|.rd$", "")
  )
}



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

