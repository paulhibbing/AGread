context("file reading method equivalence")
library(AGread)

testthat::test_that(
  "file reading methods give equivalent results", {

    ## Call the file reading methods
    test <- read_gt3x(
      system.file(
        "extdata", "example.gt3x", package = "AGread"
      )
    )
    test2 <- read_AG_raw(
      system.file(
        "extdata","exampleRAW.csv", package = "AGread"
      )
    )[ ,-c(1:2)]
    test3 <- read_AG_raw(
      system.file(
        "extdata", "exampleRAW.csv", package = "AGread"
      ),
      output_window_secs = 5
    )[ ,-c(1:2)]
    test4 <- read_AG_IMU(
      system.file(
        "extdata", "example-IMU.csv", package = "AGread"
      )
    )[ ,-c(1:2)]
    test5 <- read_AG_IMU(
      system.file(
        "extdata", "example-IMU.csv", package = "AGread"
      ),
      output_window_secs = 5
    )[ ,-c(1:2)]

    ## gt3x_raw equivalent to read_raw, 1-s epochs?
    AG <- test$RAW
    AG <- collapse_gt3x(AG)[ ,-c(1:2)]
    testthat::expect_equal(
      AG, test2, tolerance = 0.001
    )

    ## gt3x_raw equivalent to read_raw, 5-s epochs?
    AG <- test$RAW
    AG <- collapse_gt3x(
      AG, output_window_secs = 5
    )[ ,-c(1:2)]
    testthat::expect_equal(
      AG, test3, tolerance = 0.001
    )

    ## gt3x_imu equivalent to read_imu, 1-s epochs?
    AG <- test$IMU
    AG <- collapse_gt3x(AG)[ ,-c(1:2)]
    testthat::expect_equal(
      AG, test4, tolerance = 0.001
    )

    ## gt3x_imu equivalent to read_imu, 5-s epochs?
    AG <- test$IMU
    AG <- collapse_gt3x(
      AG, output_window_secs = 5
    )[ ,-c(1:2)]
    testthat::expect_equal(
      AG, test5, tolerance = 0.001
    )

  }
)
