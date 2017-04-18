context("functions work in tutorial biomass")

test_that("Wrong inputs to wsgdata_dummy throw error with informative mesages", {
  expect_error(
    wsgdata_dummy(
      bci::bci12stem1, 
      plot = 1  # wrong
    )
  )
  expect_error(
    wsgdata_dummy(
      bci::bci12stem1,
      plot = c("bci", "other")  # wrong
    )
  )
  df <- bci::bci12stem1
  df$spp <- bci::bci12stem1$sp
  df$sp <- NULL
  expect_error(wsgdata_dummy(df))
})

test_that("biomass.CTFSdb works with minimun inputs", {
  wsgdata <- wsgdata_dummy(bci::bci12full1)
  # load("../CTFSRPackage/tables/full/bci.full1.rdata")
  # wsgdata <- wsgdata_dummy(bci.full1)
  actual <- biomass.CTFSdb(
    bci::bci12stem1, 
    bci::bci12full1, 
    wsgdata = wsgdata
  )
  expect_equal_to_reference(actual, "biomass_ctfsdb.rds")
})

test_that("the output is equal of density.ind before and after removing 
  subset", {
  dold <- density.ind_old(
    bci::bci12full1, 
    "bci",
    wsgdata_dummy(bci::bci12full1)
  )
  dnew <- density.ind(
    bci::bci12full1, 
    "bci",
    wsgdata_dummy(bci::bci12full1)
  )
  expect_equal(dold, dnew)
})
