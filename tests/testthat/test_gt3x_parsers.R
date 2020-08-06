context("gt3x parsers")
library(AGread)

testthat::test_that(
  "gt3x parsers yield convergent output", {

    ## Read gt3x file
    file <- system.file(
      "extdata", "example.gt3x", package = "AGread"
    )

    testthat::expect_true(legacy_dev_compare(file))

  }
)
