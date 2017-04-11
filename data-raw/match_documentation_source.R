# Choose 10 random functions to check if documentation matches
load("C:/Users/dora/Dropbox/git_repos/CTFSRPackage/CTFSRPackage.rdata")
set.seed(4321)
sort(sample(ls(), 20))

library(tibble)

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

