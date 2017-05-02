context("complete_partial_args")

test_that("dgammaMinusdexp outputs equal when partial argument is complete", {
  # Choosing maybe crazy parameters; there was no example to follow.
  # Inside dgammaMinusdexp replaced arg log by log.p; check had detected 
  # the match was partial
  actual <- dgammaMinusdexp(
    z = c(2, 1, 1, 3, 5),
    mean = 5,
    sd = 3,
    lambda = 1/2
  )
  expect_equal_to_reference(actual, "ref_dgammaMinusdexp.rds")
})
