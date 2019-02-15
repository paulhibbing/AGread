context("gt3x file reading")
library(AGread)


# MANUAL TESTS (VERIFIED 2/15/19) -----------------------------------------

# test <- read_gt3x(
#   "inst/extdata/example.gt3x",
#   verbose = TRUE
# )
# AG <- test$RAW
# AG <- collapse_gt3x(AG)[ ,-c(1:2)]
#
# test2 <- read_AG_raw(
#   "inst/extdata/exampleRAW.csv"
# )[ ,-c(1:2)]
# > all.equal(AG, test2, tolerance = 0.001)
# [1] TRUE

# AG <- test$RAW
# AG <- collapse_gt3x(AG, output_window_secs = 5)[ ,-c(1:2)]
# test3 <- read_AG_raw(
#   "inst/extdata/exampleRAW.csv",
#   output_window_secs = 5
# )[ ,-c(1:2)]
# > all.equal(AG, test3, tolerance = 0.001)
# [1] TRUE

# test4 <- read_AG_IMU(
#   "inst/extdata/example-IMU.csv"
# )[ ,-c(1:2)]
# AG <- test$IMU
# AG <- collapse_gt3x(AG)[ ,-c(1:2)]
# > all.equal(AG, test4, tolerance = 0.001)
# [1] TRUE

# test5 <- read_AG_IMU(
#   "inst/extdata/example-IMU.csv",
#   output_window_secs = 5
# )[ ,-c(1:2)]
# AG <- test$IMU
# AG <- collapse_gt3x(
#   AG, output_window_secs = 5
# )[ ,-c(1:2)]
# > all.equal(AG, test5, tolerance = 0.001)
# [1] TRUE ##!! GIVES WARNINGS ABOUT TRUNCATION

# AUTOMATED TESTS ---------------------------------------------------------

testthat::test_that(
  "gt3x file reading and operations are consistent", {

    ## Read gt3x file
    file <- system.file(
      "extdata", "example.gt3x", package = "AGread"
    )
    test <- read_gt3x(file)

    test$RAW$Timestamp <- as.character(test$RAW$Timestamp)
    test$IMU$Timestamp <- as.character(test$IMU$Timestamp)

    ## Initial tests
    testthat::expect_equal_to_reference(
      test$RAW,
      "read_3x_raw.rds"
    )

    testthat::expect_equal_to_reference(
      test$IMU,
      "read_3x_imu.rds"
    )

    ## Re-read file (for timestamps)
    test <- read_gt3x(file)

    testthat::expect_equal_to_reference(
      collapse_gt3x(test$RAW),
      "read_3x_raw_1s.rds"
    )

    testthat::expect_equal_to_reference(
      collapse_gt3x(test$IMU),
      "read_3x_imu_1s.rds"
    )

  }
)
