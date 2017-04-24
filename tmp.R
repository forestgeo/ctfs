# Test abund.manycensus to see if it works before and after replacing 
# subset by [.

library(ctfs)
load_all()
library(bci)

allcns <- list(bci::bci12full1, bci::bci12full2)

new <- abund.manycensus(
  allcns = allcns,
  mindbh = 10,
  type = 'abund',
  excludespp = 'uniden',
  excludestatus = 'M'
)
old <- abund.manycensus_old(
  allcns = allcns,
  mindbh = 10,
  type = 'abund',
  excludespp = 'uniden',
  excludestatus = 'M'
)


all.equal(new, old)
