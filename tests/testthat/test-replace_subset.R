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
  expect_type(actual, "list")
  # Check only names are equal to reference. Bayesian approach means model
  # output will change each time, so can't check against the full object
  expect_equal_to_reference(names(actual), "ref_lmerBayes_nms.rds")
})

test_that("allquadratslopes outputs the same before and after fix", {
  # The input to elev is very specific, so it needs tweaking
  elev_tweaked <- list(col = bci::bci_elevation)
  actual <- allquadratslopes(
    elev = elev_tweaked,
    gridsize = 20,
    plotdim = c(1000, 500),
    edgecorrect = TRUE
  )
  expect_equal_to_reference(actual, "ref_allquadratslopes_old.rds")
})

test_that("allquadratslopes warns if elev is not a list with element 'col'", {
  # throws error and warning
  expect_warning(                       # capture warning
    expect_error(                       # capture error
      allquadratslopes(
        elev = bci::bci_elevation,
        gridsize = 20,
        plotdim = c(1000, 500),
        edgecorrect = TRUE
      )
    )
  )
})











test_that("spparea.sq outputs the same before and after fix", {
  # Plot is to discard, here I'm interested in sppa only.
  png("ref_spparea_sq.png")  # don't print to console
    sppa <- spparea.sq(
      bci::bci12full6,
      size = c(10, 20, 50),
      mindbh = 10,
      plotdim = c(1000, 500),
      replicates = 5,
      unidennames = c('unid')
    )
  dev.off()
  unlink("ref_spparea_sq.png")  # clean tests directory
  
  # spparea.sq uses random processess, so output is never equal. Check names.
  expect_named(sppa, c("spparea", "full"))
  expect_named(
    sppa[[1]], 
    c("xdim", "ydim", "area", "indiv", "SDindiv", "taxa", "SDtaxa")
  )
  expect_named(
    sppa[[2]], 
    c("area", "taxa", "ind")
  )
})









