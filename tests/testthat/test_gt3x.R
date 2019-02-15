context("gt3x file reading")
library(AGread)

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
      collapse_gt3x(test$RAW)[ ,-2],
      "read_3x_raw_1s.rds"
    )

    testthat::expect_equal_to_reference(
      collapse_gt3x(test$IMU)[ ,-2],
      "read_3x_imu_1s.rds"
    )

  }
)
