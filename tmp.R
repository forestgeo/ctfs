# Test abund.manycensus to see if it works before and after replacing 
# subset by [.


# pkgs --------------------------------------------------------------------

library(testthat)
library(dplyr)
load_all()

# tst data ----------------------------------------------------------------


## S3 method for class 'data'
censdata <- bci::bci12full1
split.data(
  censdata,
  splitcol = "sp",
  keepsplitcol = FALSE,
  allsplit = NULL,
  showOutput = NULL
)
