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



test_that("Chave.AGB works with inputs in example", {
  testdbh <- c(1, 2, 5, 10, 20, 30, 50, 100, 200)
  AGBmoist <- Chave.AGB(dbh = testdbh, forest = "moist")
  expect_type(AGBmoist, "double")
  expect_length(AGBmoist, 9)
  expect_equal(length(AGBmoist), length(testdbh))
})

test_that("predht.asym works as shown in examples", {
  htparam <- c(41.7, .057, .748)
  d <- c(1, 2, 5, 10, 20, 50)
  ht <- predht.asym(dbh = d, param = htparam)
  expect_type(ht, "double")
})
