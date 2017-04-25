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









test_that("individual_mort.table outputs the same before and after fix", {
  cnsdata <- list(bci::bci12full1, bci::bci12full2, bci::bci12full3)
  actual <- individual_mort.table(cnsdata)
  expect_equal_to_reference(actual, "ref_individual_mort_table_old.rds")
})


