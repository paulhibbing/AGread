context("agd file reading")
library(AGread)

## MANUAL LEGACY TEST (verified 2/14/19)
# AG_legacy <- readRDS("data-raw/counts_legacy.rds")
# AG <- read_AG_counts(
#   system.file(
#     "extdata", "example1sec.csv", package = "AGread"
#   ), verbose = TRUE, skip = 11
#   )
# > all.equal(AG, AG_legacy)
# [1] TRUE

testthat::test_that("agd file reading produces expected output", {

  agd <- system.file(
    "extdata", "example1sec.agd", package = "AGread"
  )

  csv <- system.file(
    "extdata", "example1sec.csv", package = "AGread"
  )

  test <- read_agd(agd)
  ref <- read_AG_counts(csv)

  testthat::expect_equal_to_reference(
    test, "agd_read_cache.rds"
  )

  test2 <- read_agd(agd, "settings")
  test3 <- read_agd(agd, "data")
  test4 <- read_agd(agd, "both")

  format_test <- all(
    identical(test, test3),
    identical(test, test4$data),
    identical(test2, test4$settings),
    identical(class(test), class(ref)),
    identical(class(test4$data), class(ref))
  )

  testthat::expect_true(format_test)

  testthat::expect_equal(test, ref, scale = 1)

})
