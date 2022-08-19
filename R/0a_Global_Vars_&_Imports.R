# Global Variables --------------------------------------------------------

  if(getRversion() >= "2.15.1") utils::globalVariables(c(
    ".", "block", "Date", "Last_Sample_Time", "Sample_Rate", "Start_Date",
    "Time", "Timestamp", "values", "Vector.Magnitude", "where"
  ))


  .accel_names <- c("Accelerometer_X", "Accelerometer_Y", "Accelerometer_Z")
  .gyro_names <- c("Gyroscope_X", "Gyroscope_Y", "Gyroscope_Z")
  .mag_names <- c("Magnetometer_X", "Magnetometer_Y", "Magnetometer_Z")
  .odd_value_threshold <- 20
  .packets <- c(
    "ACTIVITY", "BATTERY", "EVENT", "HEART_RATE_BPM", "LUX", "METADATA",
    "TAG", "EPOCH", "HEART_RATE_ANT", "EPOCH2", "CAPSENSE", "HEART_RATE_BLE",
    "EPOCH3", "EPOCH4", "PARAMETERS", "SENSOR_SCHEMA", "SENSOR_DATA", "ACTIVITY2"
  )


  .numbers <- as.integer(
    c(0, 2:7, 9, 11:16, 21, 24:26)
  )


# Imports -----------------------------------------------------------------

  #' @importFrom magrittr %>% %T>% %<>% %$%
  #' @importFrom rlang := .data
  #' @import Rcpp
  #' @docType package
  #' @useDynLib AGread
  NULL
