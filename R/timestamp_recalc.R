#' Re-calculate IMU timestamps after parsing and interpolating SENSOR_DATA
#' packets
#'
#' @param timestamps POSIX-formatted vector of timestamps
#' @inheritParams read_record
#' @param verbose logical. Print updates to console?
#' @param samp_rate integer. The sampling rate.
#' @param label character. The packet type
#'
#' @keywords internal
#'
timestamp_recalc <- function(
  timestamps, tz, schema, verbose, samp_rate, label
) {

  if (verbose) cat(
    "\r  Re-calculating", label, "timestamps",
    "                          "
  )

  # timestamps <- records$Timestamp
  start_time <- lubridate::force_tz(
    timestamps[1], tz
  )

  stopifnot(length(timestamps) %% samp_rate == 0)

  timestamps <- start_time +
    ((seq_along(timestamps) - 1) / samp_rate)

  if (verbose) cat(
    "\r  Re-calculating SENSOR_DATA timestamps",
    "  ............. COMPLETE               "
  )

  return(timestamps)

}

