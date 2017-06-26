
# r4ds --------------------------------------------------------------------

-- [R for Data Science, Garrett Grolemund and Hadley Wickham](http://r4ds.had.co.nz/).




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
  
  # NEEDS FUTURE WORK
  "data",  # did some; but too variable
  "export",  # did some; but too variable
  "elev",   # could do only half
  "path",  # skip, too variable
  "size",  # unclear it it's the same everywhere.
  "type",  # did some; but too variable and too little information to document
  "x", "y",  # too generic; definition changes from fun to fun
  "xrange",  # skip, too variable
  "yrange",  # skip, too variable
  
  # DECENTLY DONE
  "...",  # did many; other are weird.
  "add",  # done
  "alivecode",  # done
  "b",  # done some, others are unclear
  "badparam",  # done
  "center",  # done
  "clr",  # all done
  "dbh",  # done
  "debug",  # done
  "debunit",  # done
  "err.limit",  # done
  "fit",  # done
  "graphit",  # done
  "gridsize",  # done
  "gx",  # done
  "gy",  # done
  "lambda",  # done
  "m",  # done
  "mindbh",  # done
  "n",  # done
  "N",  # done
  "newgraph",  # done
  "outfile",  # done
  "plot",  # done
  "plotdim",  # done
  "plotside",  # done
  "ptsize",  # done
  "pts",  # done some, others vary.
  "r",  # done
  "rounddown",  # done
  "s",  # done
  "sd",  # done
  "sd1",  # done
  "start",  # done
  "steps",  # done
  "w",
  "z",  # done
  
  # SKIP FOR NOW
  "div",
  "model",
  "shape",  # too little info
  "start.param",
  "test"  # too little info
)
x <- args_count_formals_man() %>% 
  filter(!params %in% avoid) %>%
  group_by(params) %>% 
  
  mutate(some_but_not_all_is_na = some_but_not_all_is_na(man_n)) %>% 
  filter(some_but_not_all_is_na) %>% print_all()
print(x, n = x$frml_n[[1]])

# xxxnext -----------------------------------------------------------------
args_help("showstep")
args_count_param("showstep")



x %>% select(1) %>% unique()



args_count_formals_man() %>% 
  filter(is.na(man_n))
# 2017-06-16
# # A tibble: 925 x 4
#    params             fun frml_n man_n
#     <chr>           <chr>  <int> <int>
#  1      x  abundmodel.fit    133    NA
#  2      x     addBinParam    133    NA
#  3      x      AssignDiag    133    NA
#  4      x asymp.ht.fixmax    133    NA
#  5      x        asymp.ht    133    NA
#  6      x   asymptote.exp    133    NA
#  7      x    bad.binparam    133    NA
#  8      x    bad.binsdpar    133    NA
#  9      x        BadParam    133    NA
# 10      x           badSD    133    NA
# # ... with 915 more rows



# Diagnostics -------------------------------------------------------------

# xxx cont. analyse how many argument items I documented.











# Dates -------------------------------------------------------------------

library(lubridate)



ddays(18286.97)


seven %>%
  select(date) %>% 
  mutate(
    days = date %/% 1,
    days_ = date %% 1,
    hr = (days_ * 24) %/% 1,
    hr_ = (days_ * 24) %% 1,
    min = (hr_ * 60) %/% 1,
    min_ = (hr_ * 60) %% 1,
    sec = (min_ * 60) %/% 1,
    sec_ = (min_ * 60) %% 1,
    days_ = NULL,
    hr_ = NULL,
    min_ = NULL,
    sec_ = NULL,
    duration = duration(hour = hr, minute = min, second = sec),
    hours = dhours(hr)
  ) %>% 
  head()

seven %>% 
  as_tibble() %>% 
  select(date) %>% 
  mutate(
    # integer diviison to remove fraction of seconds
    duration = dseconds((date * 24 * 60 * 60) %/% 1),
    datetime = as_datetime(duration, origin = "1960-01-01")
    ) %>% 
  head()














# Unclassified ------------------------------------------------------------

# work on plotdim
args_pull_definitions("plotdim")[[2]]
# cool, there is a single definition

fun_family("allquadratslopes") %>% filter(params == "plotdim")
# There is no other function too closetly related






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

