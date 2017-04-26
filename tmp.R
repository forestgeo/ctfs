# Test abund.manycensus to see if it works before and after replacing 
# subset by [.


# pkgs --------------------------------------------------------------------

library(testthat)
load_all()

# tst data ----------------------------------------------------------------



gtable = growth.indiv(bci::bci12full6, bci::bci12full7, mindbh = 100) 

a_few_species = c(
  'termam',
  'tachve',
  'pri2co',
  'gustsu',
  'cecrin',
  'tet2pa',
  'guatdu',
  'vochfe',
  'virose',
  'maquco'
)
gtable = subset(gtable, !is.na(incgr) & sp %in% a_few_species)

modelBayes(
  data = gtable,
  ycol = 'incgr',
  xcol = 'dbh1',
  start = c(1, 0),
  startSD = 1,
  badSDparam = badSD

)








