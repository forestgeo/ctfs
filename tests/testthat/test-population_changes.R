context("functions work in tutorial population changes")

test_that(
  "graph.abundmodel works with bcixl::mod_little_gibbs", {
  expect_warning({actual <- graph.abundmodel(bcixl::mod_little_gibbs)})
  expect_type(actual, "list")
  expect_length(actual, 2)
  expect_named(actual, c("Fastest_increases", "Biggest_losses"))
})
