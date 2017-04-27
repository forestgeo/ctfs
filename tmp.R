# Test abund.manycensus to see if it works before and after replacing 
# subset by [.


# pkgs --------------------------------------------------------------------

library(testthat)
library(dplyr)
load_all()

# tst data ----------------------------------------------------------------


png("ref_spparea_sq.png")
sppa <- spparea.sq(
  bci::bci12full6,
  size = c(10, 20, 50),
  mindbh = 10,
  plotdim = c(1000, 500),
  replicates = 5,
  unidennames = c('unid')
)
dev.off()

png("ref_spparea_sq.png")
sppa1 <- spparea.sq(
  bci::bci12full6,
  size = c(10, 20, 50),
  mindbh = 10,
  plotdim = c(1000, 500),
  replicates = 5,
  unidennames = c('unid')
)
dev.off()

near(sppa, sppa1)
library(purrr)
map2(sppa, sppa1, dplyr::near)















