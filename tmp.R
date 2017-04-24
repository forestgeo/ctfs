# Test abund.manycensus to see if it works before and after replacing 
# subset by [.

library(ctfs)
load_all()
library(bci)
censdata <- bci::bci12full1


censdata = censdata
plotdim = c(1000, 500)
gridsize = 2.5
mindbh = NULL


wavelet.allsp(
  censdata = censdata,
  plotdim = c(1000, 500),
  gridsize = 2.5,
  mindbh = NULL
)
