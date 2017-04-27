# Test abund.manycensus to see if it works before and after replacing 
# subset by [.


# pkgs --------------------------------------------------------------------

library(testthat)
library(dplyr)
load_all()

# tst data ----------------------------------------------------------------

test_that("split.data outputs the same before and after fix", {
  censdata <- bci::bci12full1
  actual <- split.data(
    censdata,
    splitcol = "sp",
    keepsplitcol = FALSE,
    allsplit = NULL,
    showOutput = NULL
    expect_equal_to_reference(actual, "ref_split_data.rds")
  )
})


