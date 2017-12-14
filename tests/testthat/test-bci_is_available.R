context("bciex_is_available")

test_that("Package bciex is available", {
  expect_silent(bciex::bci12t1mini[1, 5])
})
