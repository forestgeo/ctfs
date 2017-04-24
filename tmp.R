# Test abund.manycensus to see if it works before and after replacing 
# subset by [.

library(ctfs)
load_all()
library(bci)

allcns <- list(bci::bci12full1, bci::bci12full2)


old <- wavelet.allsp(
  censdata = bci::bci12full1, plotdim=c(1000,500)
)

new <- 
  
old <- 



all.equal(new, old)
