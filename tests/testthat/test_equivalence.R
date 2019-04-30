context("file reading method equivalence")
library(AGread)

testthat::test_that(
  "file reading methods give equivalent results", {

    ## Store the file names ####

      file_3x <- system.file(
        "extdata", "example.gt3x", package = "AGread"
      )
      file_RAW <- system.file(
        "extdata","exampleRAW.csv", package = "AGread"
      )
      file_IMU <- system.file(
        "extdata", "example-IMU.csv", package = "AGread"
      )

    ## Call the file reading methods ####

      reference_3x <- read_gt3x(file_3x)

      test1 <- read_AG_raw(
        file_RAW, return_raw = TRUE
      )[ ,-c(1:2)]

      test2 <- read_AG_raw(
        file_RAW
      )[ ,-c(1:2)]

      test3 <- read_AG_raw(
        file_RAW,output_window_secs = 5
      )[ ,-c(1:2)]

      test4 <- read_AG_IMU(
        file_IMU, filter = FALSE,
        return_raw = TRUE
      )[ ,-c(1:2)]

      test5 <- read_AG_IMU(
        file_IMU
      )[ ,-c(1:2)]

      test6 <- read_AG_IMU(
        file_IMU,
        output_window_secs = 5
      )[ ,-c(1:2)]

    ## Test 1: gt3x_raw equivalent to read_raw? ####
      # (Tolerance in milli-G's)

      AG <- reference_3x$RAW
      class(AG) <- class(test1)
      accel_vars <- names(AG)[grepl("Accelerometer", names(AG))]

      testthat::expect_true(
        all(
          test_errors(AG, test1, accel_vars)
        )
      )

      testthat::expect_equal(
        AG, test1,
        tolerance = 0.001, scale = 1
      )

    ## Test 2: gt3x_raw equivalent to read_raw, 1-s epochs? ####
       # (Tolerance in milli-G's)

      AG <- reference_3x$RAW
      AG <- collapse_gt3x(AG)[ ,-c(1:2)]

      testthat::expect_true(
        all(
          test_errors(
            AG,
            test2,
            setdiff(names(AG), "Timestamp"),
            0.1
          )
        )
      )

      testthat::expect_equal(
        AG, test2,
        tolerance = 0.1, scale = 1
      )

    ## Test 3: gt3x_raw equivalent to read_raw, 5-s epochs? ####
      # (Tolerance in milli-G's)

      AG <- reference_3x$RAW
      AG <- collapse_gt3x(
        AG, output_window_secs = 5
      )[ ,-c(1:2)]

      testthat::expect_true(
        all(
          test_errors(
            AG,
            test3,
            setdiff(names(AG), "Timestamp"),
            0.1
          )
        )
      )

      testthat::expect_equal(
        AG, test3,
        tolerance = 0.1, scale = 1
      )

    ## Test 4: gt3x_imu equivalent to read_imu? ####

      AG <- reference_3x$IMU
      test4 <- test4[ ,names(AG)]
      class(AG) <- class(test4)
      gyro_names <- names(AG)[
        grepl("Gyroscope", names(AG))
      ]

      testthat::expect_true(
        all(
          test_errors(
            AG, test4, gyro_names,
            tolerance = 0.5
          )
        )
      )

      testthat::expect_equal(
        AG[ ,gyro_names], test4[ ,gyro_names],
        tolerance = 0.05, scale = 1
      )
      ## ^^ Tolerance here is acceptable for degrees per second,
      ## but not other variables (see below)

      testthat::expect_equal(
        AG[ ,!gyro_names], test4[ ,!gyro_names],
        tolerance = 0.001, scale = 1
      )
      ## ^^ Lower tolerance here because it's raw values
      ## (e.g. raw G's) rather than scaled (e.g. ENMO
      ## in milli-G's)

    ## Test 5: gt3x_imu equivalent to read_imu, 1-s epochs? ####

      AG <- reference_3x$IMU
      AG <- collapse_gt3x(AG)[ ,-c(1:2)]

      gyro_names <- grepl("Gyroscope", names(AG))

      testthat::expect_equal(
        AG[ ,!gyro_names], test5[ ,!gyro_names],
        tolerance = 0.05, scale = 1
      )
      testthat::expect_equal(
        AG[ ,!gyro_names], test5[ ,!gyro_names],
        tolerance = 0.001, scale = 1
      )

    ## Test 6: gt3x_imu equivalent to read_imu, 5-s epochs? ####
      # (Same tolerance notes as previous section)

      AG <- reference_3x$IMU
      AG <- collapse_gt3x(
        AG, output_window_secs = 5
      )[ ,-c(1:2)]

      testthat::expect_equal(
        AG[ ,gyro_names], test6[ ,gyro_names],
        tolerance = 0.05, scale = 1
      )
      testthat::expect_equal(
        AG[ ,!gyro_names], test6[ ,!gyro_names],
        tolerance = 0.001, scale = 1
      )

  }
)
