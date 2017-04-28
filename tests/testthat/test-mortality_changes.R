context("functions work in tutorial mortality changes")

test_that(
  "individual_mort.table works with census bci12full1 and bci12full15", {
    actual <- individual_mort.table(
      list(
        bci::bci12full1, 
        bci::bci12full5
      )
    )
    expect_equal_to_reference(actual, "ref_individual_mort_table.rds")
    expect_is(actual, "data.frame")
  }
)

test_that(
  "calcMortIndivTable works with output of individual_mort.table with census
    bci12full1 and bci12full15", {
      mtable <- individual_mort.table(list(bci::bci12full1, bci::bci12full5))
      actual <- calcMortIndivTable(mtable, by = "species")
      expect_equal_to_reference(actual, "ref_calcMortIndivTable.rds")
      expect_is(actual, "matrix")
  }
)

test_that(
  "growth.indiv works with bci12full6 and bci12full7 with defaults", {
    actual <- growth.indiv(bci::bci12full6, bci::bci12full7)
    expect_equal_to_reference(actual, "ref_growth_indiv.rds")
  }
)
