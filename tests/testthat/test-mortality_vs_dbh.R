context("functions work in tutorial mortality vs dbh")

test_that(
  "mortality.eachspp works with census bci12full1 and bci12full15 & defaults", {
  actual <- mortality.eachspp(
    census1 = bci::bci12full1, 
    census2 = bci::bci12full5
  )
  expect_equal_to_reference(actual, "ref_mortality_eachspp.rds")
})





