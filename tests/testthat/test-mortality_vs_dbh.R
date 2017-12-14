context("functions work in tutorial mortality vs dbh")

test_that(
  "mortality.eachspp works with census 1 and 5 & defaults", {
  actual <- mortality.eachspp(
    census1 = bciex::bci12t1mini, 
    census2 = bciex::bci12t5mini
  )
  expect_equal_to_reference(actual, "ref_mortality_eachspp.rds")
})





