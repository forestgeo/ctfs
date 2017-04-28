## Not run:




# library(splancs)

# sum consp and hetsp neighbors for all stems in the plot
neighbor.counts <- NeighborDensities(bci::bci12full7, r = 20, type = 'count')

sum consp and hetsp neighbors for only one species:one.sp = subset(bci.full7, sp ==
    "quaras")
neighbor.counts <-
  NeighborDensities(bci.full7, one.sp, type = 'count')
## End(Not run)
