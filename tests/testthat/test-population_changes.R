context("functions work in tutorial population changes")

test_that("model.littleR.Gibbs works with bci12full1 and 5 & defaults", {
  # errs because argument "sptable" is missing, with no default
  library(date)
  expect_error({
    model.littleR.Gibbs(
      cns1 = bci::bci12full1,
      cns2 = bci::bci12full5,
      mindbh = 1
    )
  })
  # solves by providing sptable
  path <- "../bci16eg/data/bci.spptable.rda"
  load(path)
    model.littleR.Gibbs(
    cns1 = bci::bci12full1,
    cns2 = bci::bci12full5,
    sptable = bci.spptable,
    mindbh = 1
  )
})
