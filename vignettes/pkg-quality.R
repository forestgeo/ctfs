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

## ---- echo=FALSE, message=FALSE------------------------------------------
# errors ----
err001 <- "Error in llike.model.lmer, function 'dmvnorm' was missing"
err002 <- "Error in tojulian, package date was missing"
err003 <- "Errs because uses subset, i.e. non-standard evaluation"
err004 <- "Errs because density_ind argument wsgdata is missing"
err005 <- paste0(err004, " and could also err because density_ind used subset")
err006 <- "Called from: linear.model. Error in x %*% b : requires numeric/complex matrix/vector arguments"
err07 <- "need output of run.growthbin.manyspp"
err008 <- "mandatory input files are not provided for examples"
err009 <- "Error in subset.default(sppdata, gx >= 0 & gy >= 0 & gx < plotdim[1] &  : object 'gx' not found"
err010 <- "Lacks a survey data object to produce, via rearrangeSurveyData, the main data that this function requires"



# solutions ----
solved_with <- tibble::tribble(
  ~fun, ~msg, ~guess_solution,
  "lmerBayes", err001, "use_package('mvtnorm'), use_package('MCMCpack')",
  "mortality.eachspp", err002, "use_package('date')",
  "model.littleR.Gibbs", err003, "remove subset (non-standard evaluation)",
  "biomass.CTFSdb", err004, "provide argument wsgdata with wsgdata_dummy()",
  "density_ind", err005, "remove subset (non-standard evaluation)",
  "growth.flexbin", err006, "Unsolved. I need to traceback. problem likely in linear.model.ctr.",
  "run.growthfit.bin", err006, "Unsolved. I need to traceback. problem likely in linear.model.ctr.",
  "run.growthbin.manyspp", err006, "Unsolved. I need to traceback. problem likely in linear.model.ctr.",
  "graph.growthmodel.spp", err07, "unsolved",
  "compare.growthbinmodel", err07, "unsolved",
  "overlay.growthbinmodel", err07, "unsolved",
  "fullplot.imageJ", err008, "unsolved. Should give example files for examples",
  "pdf.allplot", err009, "unsolved. Try replacing subset by nse equivalent",
  "png.allplot", err009, "unsolved. Try replacing subset by nse equivalent",
  "solve_topo", err010, "unsolved. Should provide an example survey object"
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
  "Biomass", "predht.asym", F,
  "Growth vs. DBH", "extract.growthdata", F,
  "Growth vs. DBH", "growth.flexbin", T,
  "Growth vs. DBH", "run.growthfit.bin", T,
  "Growth vs. DBH", "run.growthbin.manyspp", T,
  "Growth vs. DBH", "graph.growthmodel.spp", T,
  "Growth vs. DBH", "compare.growthbinmodel", T,
  "Growth vs. DBH", "overlay.growthbinmodel", T,
  "Plot maps", "map", F,
  "Plot maps", "pdf.allplot", T,
  "Plot maps", "png.allplot", T,
  "Plot maps", "complete.plotmap", F,
  "Topography", "rearrangeSurveyData", F,
  "Topography", "solve_topo", T
)

## ------------------------------------------------------------------------
errs %>% count(errs)

## ------------------------------------------------------------------------
dplyr::left_join(errs, solved_with) %>% 
  filter(errs) %>% 
  print() %>%
  select(msg)

