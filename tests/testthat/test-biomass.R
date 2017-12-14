context("functions work in tutorial biomass")

test_that("Wrong inputs to wsgdata_dummy throw error with informative mesages", {
  expect_error(
    wsgdata_dummy(
      bciex::bci12s1mini, 
      plot = 1  # wrong
    )
  )
  expect_error(
    wsgdata_dummy(
      bciex::bci12s1mini,
      plot = c("bci", "other")  # wrong
    )
  )
  df <- bciex::bci12s1mini
  df$spp <- bciex::bci12s1mini$sp
  df$sp <- NULL
  expect_error(wsgdata_dummy(df))
})

test_that("biomass.CTFSdb works with minimun inputs", {
  wsgdata <- wsgdata_dummy(bciex::bci12t1mini)
  # load("../CTFSRPackage/tables/full/bci.full1.rdata")
  # wsgdata <- wsgdata_dummy(bci.full1)
  actual <- biomass.CTFSdb(
    bciex::bci12s1mini, 
    bciex::bci12t1mini, 
    wsgdata = wsgdata
  )
  expect_equal_to_reference(actual, "ref_biomass_ctfsdb.rds")
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
