context("RAW data processing")
library(AGread)

## MANUAL LEGACY TEST (verified 2/14/19)
# AG_legacy <- readRDS("data-raw/RAW_legacy.rds")
# AG <- read_AG_raw(
#   system.file(
#     "extdata", "TestID_LeftWrist_RAW.csv", package = "AGread"
#   )
#   )[ ,-2]
# > all.equal(AG, AG_legacy)
# [1] TRUE

testthat::test_that("File reading produces expected output", {
  testthat::expect_equal_to_reference(
    read_AG_raw(
      system.file(
        "extdata", "TestID_LeftWrist_RAW.csv", package = "AGread"
      )
    )[ ,-2],
    "RAW_read_cache_1.rds"
  )
  testthat::expect_equal_to_reference(
    read_AG_raw(
      system.file(
        "extdata", "TestID_LeftWrist_RAW.csv", package = "AGread"
      ),
      5
    )[ ,-2],
    "RAW_read_cache_5.rds"
  )
})
