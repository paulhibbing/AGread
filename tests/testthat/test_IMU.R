context("IMU data processing")
library(AGread)

## MANUAL LEGACY TEST (verified 2/14/19)
# AG_legacy <- readRDS("data-raw/IMU_legacy.rds")
# AG <- read_AG_IMU(
#   system.file(
#     "extdata", "example-IMU.csv", package = "AGread"
#   ),
#   output_vars = c("gyroscope", "magnetometer")
#   )[ ,-2]
# > all.equal(AG, AG_legacy)
# [1] TRUE

testthat::test_that("File reading produces expected output", {
  testthat::expect_equal_to_reference(
    read_AG_IMU(
      system.file(
        "extdata", "example-IMU.csv", package = "AGread"
      ),
      output_vars = c("gyroscope", "magnetometer")
    )[ ,-2],
    "IMU_read_cache.rds"
  )
})
