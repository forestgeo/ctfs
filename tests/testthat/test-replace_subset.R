context("Replace subset by [")

test_that("abund.manycensus outputs the same before and after fix", {
  allcns <- list(bci::bci12full1, bci::bci12full2)
  actual <- abund.manycensus(
      allcns = allcns,
      mindbh = 10,
      type = 'abund',
      excludespp = 'uniden',
      excludestatus = 'M'
  )
  expect_equal_to_reference(actual, "ref_abund_manycensus.rds")
})

test_that("individual_grow.table outputs the same before and after fix", {
  cnsdata <- list(bci::bci12full1, bci::bci12full2, bci::bci12full3)
  actual <- individual_grow.table(cnsdata)
  expect_equal_to_reference(actual, "ref_individual_grow_table_old.rds")
})

test_that("extract.growthdata outputs the same before and after fix", {
  actual <- extract.growthdata(bci::bci12full1, bci::bci12full2)
  expect_equal_to_reference(actual, "ref_extract_growthdata_old.rds")
})













test_that("lmerBayes outputs the same before and after fix", {
  library(MCMCpack)
  library(mvtnorm)

  gtable <- growth.indiv(bci::bci12full1, bci::bci12full7, mindbh = 100)
  a_few_species <- c('termam', 'tachve', 'pri2co', 'gustsu', 'cecrin', 'tet2pa',
    'guatdu', 'vochfe', 'virose', 'maquco')
  gtable <- subset(gtable, !is.na(incgr) & sp %in% a_few_species)
  
  actual <- lmerBayes(
    data = gtable,
    ycol = 'incgr',
    xcol = 'dbh1',
    randcol = 'sp',
    start = c(1, 0),
    startSD = 1,
    startCov = 1,
    model = linear.model,
    error = 'Gauss',
    includeCovar = FALSE,
    badSDparam = badSD,
    steps = 500,     # low to save time
    showstep = 500,  # low to save time
    burnin = 100
  )
expect_equal_to_reference(actual, "ref_lmerBayes_old.rds")
})

xxx now replace subset by [



