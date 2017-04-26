# Test abund.manycensus to see if it works before and after replacing 
# subset by [.


# pkgs --------------------------------------------------------------------

library(testthat)
load_all()

# tst data ----------------------------------------------------------------



library(splancs)

# sum consp and hetsp neighbors for only one species:
data <- bci::bci12full7
one.sp = subset(data, sp == "quaras")
neighbor.counts <- NeighborDensities(data, one.sp, type = 'count')

head(neighbor.counts_old)
head(neighbor.counts)
all.equal(neighbor.counts_old, neighbor.counts)




