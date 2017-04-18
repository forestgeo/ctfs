# context("functions work in tutorial biomass")
# 
# test_that("biomass.CTFSdb works with bci12stem1 and bci12full1", {
#   # actual <- biomass.CTFSdb(bci::bci12stem1, bci::bci12full1)
#   expect_equal_to_reference(actual, "biomass_ctfsdb.rds")
# })



library(testthat)
library(ctfs)
library(bci)
load("../CTFSRPackage/tables/stem/bci.stem1.rdata")
load("../CTFSRPackage/tables/full/bci.full1.rdata")

names(bci.full1)
names(bci.stem1)

actual <- biomass.CTFSdb(bci.stem1, bci.full1)

