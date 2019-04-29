fix_usb <- function(Type, verbose = FALSE, ...) {

  if (verbose) cat(
    "\r  Checking/addressing USB connection time(s)",
    "                                              "
  )

  switch(
    Type,
    "25" = imu_fix_usb(...),
    "26" = accel_fix_usb(...)
  )

}

#' Address breaks in the primary accelerometer data stream due to device USB
#' connection
#'
#' @inheritParams zero_fill
#' @inheritParams read_record
#'
#' @keywords internal
accel_fix_usb <- function(records, info) {

  missing_records <- sapply(
    records,
    function(x) all(is.na(x$Payload))
  )

  missing_records <- which(missing_records)

  if (!length(missing_records)) return(records)

  records[[missing_records]] <- do.call(
    c,
    lapply(
      missing_records, latch_value, records = records
    )
  )

  insert_zero_runs(records, missing_records)

}

#' Address breaks in the IMU data stream due to device USB connection
#'
#' @inheritParams zero_fill
#'
#' @keywords internal
imu_fix_usb <- function(records) {

  time_gaps <- diff(
    sapply(
      records,
      function(x) x$Timestamp,
      USE.NAMES = FALSE
    )
  )

  if (all(time_gaps == 1)) return(records)

  gap_indices <- which(time_gaps != 1)

  insert_zero_runs(records, gap_indices)

}
