#' Format columns in collapsed IMU data
#'
#' @param AG data frame containing IMU data
#' @inheritParams read_AG_IMU
#' @param samp_rate integer. The sampling rate.
#' @param block_size integer. Rows of raw data per output window
#'
#' @keywords internal
#'
ag_imu_format <- function(
  AG, output_window_secs = 1, filter, samp_rate,
  filter_hz = 35, verbose, block_size
) {

  AG <- check_second(AG)
  if (filter) {
    AG <- imu_filter_gyroscope(
      AG, samp_rate, filter_hz, verbose
    )
  }

  # Calculate vector magnitudes
  if (verbose) message_update(21)

  accel_test <- grepl(
    "accelerometer", names(AG), ignore.case = TRUE
  )
  if (any(accel_test)) {
    AG$mean_Accel_VM <- get_VM(
      AG[, accel_test],
      verbose = verbose
    )
  }

  gyro_test <- grepl(
    "gyroscope", names(AG), ignore.case = TRUE
  )
  if (any(gyro_test)) {
    AG$Gyroscope_VM_DegPerS <- get_VM(
      AG[, gyro_test],
      verbose = verbose
    )
  }

  mag_test <- grepl(
    "magnetometer", names(AG), ignore.case = TRUE
  )
  if (any(mag_test)) {
    AG$Magnetometer_VM_MicroT <- get_VM(
      AG[,mag_test],
      verbose = verbose
    )
  }

  AG <- imu_collapse(
    AG, block_size, verbose = verbose
  )

  first_variables <- c(
    "file_source_IMU", "date_processed_IMU", "Timestamp"
  )

  AG <- AG[, c(first_variables, setdiff(names(AG), first_variables))]

  AG$epoch <- NULL

  names(AG) <- gsub(
    "mean_Gyroscope_VM_DegPerS",
    "Gyroscope_VM_DegPerS",
    names(AG)
  )

  return(AG)

}
