context("functions work in tutorial mortality changes")

test_that(
  "individual_mort.table works with census bci12full1 and bci12full15", {
    actual <- individual_mort.table(
      list(
        bci::bci12full1, 
        bci::bci12full5
      )
    )
    expect_equal_to_reference(actual, "ref_individual_mort.table.rds")
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

test_that(
  "lmerBayes works as in example in the original source", {
    gtable <- growth.indiv(bci::bci12full6, bci::bci12full7, mindbh=100)
    a_few_species <- c(
      "termam", "tachve", "pri2co", "gustsu", "cecrin", "tet2pa", "guatdu",
      "vochfe", "virose", "maquco"
    )
    gtable <- subset(gtable, !is.na(incgr) & sp %in% a_few_species)
    # Capture expresison to reuse later, don't run yet.
    mod <- substitute(
      lmerBayes(
        data = gtable,
        ycol = 'incgr',
        xcol = 'dbh1',
        randcol = 'sp',
        start = c(1, 0),
        startSD = 1,
        startCov = 1,
        model = linear.model,
        error = 'Gauss',
        includeCovar = FALSE,
        badSDparam = badSD,
        steps = 1100,
        showstep = 50,
        burnin = 100
      )
    )
    # Errs because it needs mvtnorm::dmvnorm() and MCMCpack::rinvgamma()
    expect_error(eval(mod))
    # Dissabled because it takes too long ---
    # # Load dependencies and retry
    # library(mvtnorm)
    # library(MCMCpack)
    # modlist <- eval(mod)
    # expect_type(modlist, "list")
    # expect_equal_to_reference(names(modlist), "ref_lmerBayes_nms.rds")
  }
)
