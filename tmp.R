# Test abund.manycensus to see if it works before and after replacing 
# subset by [.


# pkgs --------------------------------------------------------------------

library(testthat)
load_all()

# tst data ----------------------------------------------------------------

gtable = growth.indiv(bci::bci12full1, bci::bci12full2, mindbh = 100)
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


data = gtable
ycol = 'incgr'
xcol = 'dbh1'
randcol = 'sp'
start = c(1, 0)
startSD = 1
startCov = 1
model = linear.model
error = 'Gauss'
includeCovar = FALSE
badSDparam = badSD
steps = 1100
showstep = 50
burnin = 100



data_old=subset(data,!is.na(data[,randcol]))





all.equal(data_old, data)






