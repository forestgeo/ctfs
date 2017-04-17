context("functions work in tutorial population changes")

test_that("model.littleR.Gibbs works with bci12full1 and 5 & defaults", {
  # errs because package date is missing
  # ctfs must import date; solve with use_package("date", "Depends")
  expect_error(
    model.littleR.Gibbs(
      cns1 = bci::bci12full1,
      cns2 = bci::bci12full5,
      mindbh = 1
    )
  )
  # errs because 
  library(date)
  expect_error({
    model.littleR.Gibbs(
      cns1 = bci::bci12full1,
      cns2 = bci::bci12full5,
      mindbh = 1
    )
  })
})
