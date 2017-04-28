## Not run:




# library(splancs)

one.sp = subset(bci::bci12full7, sp == "quaras")
neighbor.counts <- NeighborDensities(bci::bci12full7, one.sp, type = 'count')
