context("functions work in tutorial growth changes")


test_that(
  "individual_grow.table works with census bci12full1 and bci12full15", {
    actual <- individual_grow.table(
      cnsdata = list(
        bciex::bci12t1mini, 
        bciex::bci12t5mini
      )
    )
    expect_equal_to_reference(actual, "ref_individual_grow_table.rds")
    expect_is(actual, "data.frame")
})

test_that(
  "pospower works with positive and negative inputs, integer or real", {
    expected_if_positive <- 4
    actual <- pospower(2, 2)
    expect_equal(actual, expected_if_positive)
    
    expected_if_negative <- -4
    actual <- pospower(-2, 2)
    expect_equal(actual, expected_if_negative)
    
    expected_if_positive_real <- 4.84
    actual <- pospower(2.2, 2)
    expect_equal(actual, expected_if_positive_real)

    expected_if_negative_real <- -4.84
    actual <- pospower(-2.2, 2)
    expect_equal(actual, expected_if_negative_real)
})
