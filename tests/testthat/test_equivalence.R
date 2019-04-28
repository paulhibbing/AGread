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
       # (Tolerance in milli-G's)
    AG <- test$RAW
    AG <- collapse_gt3x(AG)[ ,-c(1:2)]

    if (nrow(AG) - nrow(test2) == 1) {
      AG <- AG[-nrow(AG), ]
    }

    testthat::expect_equal(
      AG, test2,
      tolerance = 0.1, scale = 1
    )

    ## gt3x_raw equivalent to read_raw, 5-s epochs?
      # (Tolerance in milli-G's)
    AG <- test$RAW
    AG <- collapse_gt3x(
      AG, output_window_secs = 5
    )[ ,-c(1:2)]

    if (nrow(AG) - nrow(test3) == 1) {
      AG <- AG[-nrow(AG), ]
    }

    testthat::expect_equal(
      AG, test3,
      tolerance = 0.1, scale = 1
    )

    ## gt3x_imu equivalent to read_imu, 1-s epochs?
    AG <- test$IMU
    AG <- collapse_gt3x(AG)[ ,-c(1:2)]

    gyro_names <- grepl("Gyroscope", names(AG))

    testthat::expect_equal(
      AG[ ,!gyro_names], test4[ ,!gyro_names],
      tolerance = 0.05, scale = 1
    )
    ## ^^ Tolerance here is acceptable for degrees per second, but not other
    ## variables (see below)

    testthat::expect_equal(
      AG[ ,!gyro_names], test4[ ,!gyro_names],
      tolerance = 0.001, scale = 1
    )
    ## ^^ Lower tolerance here because it's raw values (e.g. raw G's) rather
    ## than scaled (e.g. ENMO in milli-G's)

    ## gt3x_imu equivalent to read_imu, 5-s epochs?
      # (Same tolerance notes as previous section)
    AG <- test$IMU
    AG <- collapse_gt3x(
      AG, output_window_secs = 5
    )[ ,-c(1:2)]

    testthat::expect_equal(
      AG[ ,gyro_names], test5[ ,gyro_names],
      tolerance = 0.05, scale = 1
    )
    testthat::expect_equal(
      AG[ ,!gyro_names], test5[ ,!gyro_names],
      tolerance = 0.001, scale = 1
    )

  }
)
