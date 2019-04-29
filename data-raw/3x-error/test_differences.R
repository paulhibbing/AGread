rm(list = ls())
devtools::load_all()
library(ggplot2)

## Store the file names ####

  file_3x <- system.file(
    "extdata", "example.gt3x", package = "AGread"
  )
  file_IMU <- system.file(
    "extdata", "example-IMU.csv", package = "AGread"
  )

## Call the file reading methods ####

  reference_3x <- read_gt3x(file_3x)

  test4 <- read_AG_IMU(
    file_IMU, filter = FALSE,
    return_raw = TRUE
  )[ ,-c(1:2)]

## Test 4: gt3x_imu equivalent to read_imu? ####

  AG <- reference_3x$IMU
  test4 <- test4[ ,names(AG)]
  class(AG) <- class(test4)
  gyro_names <- grepl("Gyroscope", names(AG))

  all.equal(
    AG[ ,gyro_names], test4[ ,gyro_names],
    scale = 1
  )
  all.equal(
    AG[ ,!gyro_names], test4[ ,!gyro_names],
    scale = 1, tolerance = 0.001
  )


## Look at differences ####

  diff_test <- function(variable) {

    print("New test")

    diffs <- sqrt(
      (AG[ ,variable] - test4[ ,variable])^2
    )

    cat(paste("\n\nMax difference:", max(diffs)))
    max_index <- which.max(diffs)

    cat(
      "\n\nread_gt3x value:", AG[max_index,variable],
      "\nread_AG_IMU value:", test4[max_index,variable]
    )

    cat(
      "\n\nTimestamp:",
      strftime(
        AG$Timestamp[max_index], "%H:%M:%OS3", "UTC"
      )
    )

    cat(
      "\n\nPacket number:",
      floor(max_index/100)
    )

  }

  diff_test("Gyroscope_Y")

## Plot differences ####

  plot_data <- data.frame(
    x = AG[ ,variable], y = test4[ ,variable]
  )
  ggplot(plot_data, aes(x,y)) +
    geom_point() +
    stat_function(fun = function(x) x, size = 1.2)
