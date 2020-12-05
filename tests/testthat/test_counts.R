context("Count data processing")
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

testthat::test_that("csv file reading produces expected output", {
  testthat::expect_equal_to_reference(
    read_AG_counts(
      system.file(
        "extdata", "example1sec.csv", package = "AGread"
      ), skip = 11
    ),
    "counts_read_cache.rds"
  )
})
