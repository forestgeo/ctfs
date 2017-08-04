context("bci_is_available")

test_that("Package bci is available", {
  expect_silent(bci::bci12full1[1, 5])
})
