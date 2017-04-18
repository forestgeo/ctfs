## ---- echo = FALSE, message=FALSE, warning=FALSE-------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE
)
library(tibble)
library(dplyr)
library(sig)

## ------------------------------------------------------------------------
sig::sig_report(pkg2env(ctfs))

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
# errors ----
err001 <- "Error in llike.model.lmer, function 'dmvnorm' was missing"
err002 <- "Error in tojulian, package date was missing"
err003 <- "Errs because uses subset, i.e. non-standard evaluation"
err004 <- "Errs because density.ind argument wsgdata is missing"
err005 <- paste0(err004, " and could also err because density.ind used subset")

# solutions ----
solved_with <- tibble::tribble(
  ~fun, ~msg, ~guess_solution,
  "lmerBayes", err001, "use_package('mvtnorm'), use_package('MCMCpack')",
  "mortality.eachspp", err002, "use_package('date')",
  "model.littleR.Gibbs", err003, "remove subset (non-standard evaluation)",
  "biomass.CTFSdb", err004, "provide argument wsgdata with wsgdata_dummy()",
  "density.ind", err005, "remove subset (non-standard evaluation)"
)

# functions that err ----
errs <- tibble::tribble(
  ~tutorial, ~fun, ~errs,
  "Growth changes", "individual_grow.table", F,
  "Growth changes", "pospower", F,
  "Mortality changes", "individual_mort.table", F,
  "Mortality changes", "calcMortIndivTable", F,
  "Mortality changes", "growth.indiv", F,
  "Mortality changes", "lmerBayes", T,
  "Mortality vs. dbh", "mortality.eachspp", T,
  "Population Changes", "model.littleR.Gibbs", T,
  "Population Changes", "graph.abundmodel", F,
  "Population Changes", "fitSeveralAbundModel", F,
  "Biomass", "CTFSplot", F,
  "Biomass", "attach_if_needed", F,
  "Biomass", "biomass.CTFSdb", T,
  "Biomass", "Chave.AGB", F,
  "Biomass", "predht.asym", F
  # "Growth vs. DBH",
  # "ImageJ",
  # "Plot maps",
  # "Topography",
)
errs
dplyr::left_join(errs, solved_with) %>% filter(errs)

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

