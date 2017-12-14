context("functions work in tutorial mortality changes")

test_that(
  "individual_mort.table works with census bci12full1 and bci12full15", {
    actual <- individual_mort.table(
      list(
        bciex::bci12t1mini, 
        bciex::bci12t5mini
      )
    )
    expect_equal_to_reference(actual, "ref_individual_mort_table.rds")
    expect_is(actual, "data.frame")
  }
)

test_that(
  "calcMortIndivTable works with output of individual_mort.table with census
    bci12full1 and bci12full15", {
      mtable <- individual_mort.table(list(bciex::bci12t1mini, bciex::bci12t5mini))
      actual <- calcMortIndivTable(mtable, by = "species")
      expect_equal_to_reference(actual, "ref_calcMortIndivTable.rds")
      expect_is(actual, "matrix")
  }
)

test_that(
  "growth.indiv works with bci12full6 and bci12full7 with defaults", {
    actual <- growth.indiv(bciex::bci12t6mini, bciex::bci12t7mini)
    expect_equal_to_reference(actual, "ref_growth_indiv.rds")
  }
)
