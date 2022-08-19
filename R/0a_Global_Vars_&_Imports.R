# Global Variables --------------------------------------------------------

  if(getRversion() >= "2.15.1") utils::globalVariables(c(
    ".", "block", "Date", "Last_Sample_Time", "Sample_Rate", "Start_Date",
    "Time", "Timestamp", "values", "Vector.Magnitude", "where"
  ))


  ## Variable names

    .accel_names <- c("Accelerometer_X", "Accelerometer_Y", "Accelerometer_Z")
    .gyro_names <- c("Gyroscope_X", "Gyroscope_Y", "Gyroscope_Z")
    .mag_names <- c("Magnetometer_X", "Magnetometer_Y", "Magnetometer_Z")


  ## general gt3x parsing

    .odd_value_threshold <- 20
    .packets <- c(
      "ACTIVITY", "BATTERY", "EVENT", "HEART_RATE_BPM", "LUX", "METADATA",
      "TAG", "EPOCH", "HEART_RATE_ANT", "EPOCH2", "CAPSENSE", "HEART_RATE_BLE",
      "EPOCH3", "EPOCH4", "PARAMETERS", "SENSOR_SCHEMA", "SENSOR_DATA", "ACTIVITY2"
    )
    .numbers <- as.integer(
      c(0, 2:7, 9, 11:16, 21, 24:26)
    )


  ## lux parsing

    .lux_scale <- c(
      "NEO" = 1.25, "CLE" = 1.25, "MRA" = 3.25,
      "MOS0" = NA, "MOS1" = 2.05, "MOS2" = 2.05,
      "MOS3" = 2.67, "MOS4" = 2.67
    )
    .lux_max <- c(
      "NEO" = 2500, "CLE" = 2500, "MRA" = 6000,
      "MOS0" = NA, "MOS1" = 5000, "MOS2" = 5000,
      "MOS3" = 6500, "MOS4" = 6500
    )

  ## agd reading and activity count operations

    .triaxial_vars <- c("Axis1", "Axis2", "Axis3")

    .agd_names <- data.frame(
      agd_name = c(
        "dataTimestamp", "axis1", "axis2", "axis3", "steps", "lux",
        "inclineOff", "inclineStanding", "inclineSitting", "inclineLying"
      ),
      csv_name = c(
        "Timestamp", .triaxial_vars, "Steps", "Lux",
        paste0("Inclinometer.", c("Off", "Standing", "Sitting", "Lying"))
      ),
      stringsAsFactors = FALSE
    )


# Imports -----------------------------------------------------------------

  #' @importFrom magrittr %>% %T>% %<>% %$%
  #' @importFrom rlang := .data
  #' @import Rcpp
  #' @docType package
  #' @useDynLib AGread
  NULL
