## ---- echo = FALSE, message=FALSE, warning=FALSE-------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE
)
library(tibble)

## ------------------------------------------------------------------------
# install.packages(sig)
library(sig)
sig_report(pkg2env(ctfs))

## ------------------------------------------------------------------------
sig_report(pkg2env(Hmisc))

## ------------------------------------------------------------------------
sig_report(pkg2env(assertive))

## ------------------------------------------------------------------------
sig_report(pkg2env(testthat))

## ---- echo=FALSE---------------------------------------------------------
tribble(
  ~fun, ~match_source, ~match_webpage,
  "arrangeParam.llike.2D", T, T,
  "attach_if_needed", T, T,
  "biomass.growth", T, T,
  "border.distance", T, T,
  "drawrectangle", T, T,
  "exponential.sin", T, T,
  "find.xaxis.hist", T, T,
  "graph.mvnorm", T, T,
  "linear.mortmodel", T, T,
  "logistic.power.mode", T, T,
  "pospower", T, T,
  "pts.to.interceptslope", T, T,
  "recalculate.lmerBayesllike", T, T,
  "saveParamFile", T, T,
  "skewness", T, T, 
  "summaryMCMC", T, T
)

## ------------------------------------------------------------------------
tribble(
  ~tutorial, ~fun, ~errs,
  "Growth changes", "individual_grow.table", F,
  "Growth changes", "pospower", F
  # "Mortality changes",
  # "Mortality vs. dbh",
  # "Population Changes",
  # "Biomass",
  # "Growth vs. DBH",
  # "ImageJ",
  # "Plot maps",
  # "Topography",

)

## ---- include=FALSE------------------------------------------------------
# # Sample 30 functions at random (must run, not knit)
# set.seed(4435)
# library(dplyr)
# 
# tibble::tibble(fun = dir("./man/")) %>% 
#   dplyr::mutate(
#     fun = stringr::str_replace(fun, ".Rd$", "")
#   ) %>% 
#   dplyr::sample_n(30) %>% 
#   dplyr::pull()

