# Choose 10 random functions to check if documentation matches
load("C:/Users/dora/Dropbox/git_repos/ctfs/data/CTFSRPackage.rdata")
set.seed(1234)
sort(sample(ls(), 20))

library(tibble)

checks <- tribble(
  ~fun, ~match_source, ~match_webpage,
  "abundmodel.fit", T, T,
  "calc.directionslope", T, T,
  "countEmpty", T, T,
  "defineBinBreaks", T, T,
  "density.ind", T, T,
  "dgamma.mean", T, T,
  "index.to.gxgy", T, T,
   "is.between", T, T,
  "logistic.power.mode", T, T,
  "logit", T, T,
  "map", T, T,
  "metrop1step", T, T,
  "mortality", F, F, 
  "selectrandomquad2", T, T
)
checks

problems <- tribble(
  ~fun, ~note,
  "mortality", "Some <li> itmes maped to arguments should not be arguments."
)
problems


not_function <- c(
  "CTFSpath", "CTFSworkshop", "defineBinBreaks", "dendrometer", 
  "remove.allpackage", "sampledigit"
)
not_function

# Todo

# Make "not arguments" to <li> items outside <arguments>. Items <li> map to arguments only if inside <arguments>, else, they are list items for other purposes, not arguments. For example, mortality.
