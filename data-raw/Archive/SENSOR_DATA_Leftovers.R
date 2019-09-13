#' Re-calculate IMU timestamps after parsing and interpolating SENSOR_DATA
#' packets
#'
#' @param timestamps POSIX-formatted vector of timestamps
#' @inheritParams read_gt3x
#' @param samp_rate integer. The sampling rate.
#' @param label character. The packet type
#'
#' @keywords internal
#'
timestamp_recalc <- function(
  timestamps, tz, verbose, samp_rate, label
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
    "\r  Re-calculating", label, "timestamps",
    "  ............. COMPLETE               "
  )

  return(timestamps)

}


SENSOR_DATA_extras <- function(
  records, schema, tz, label, verbose
) {

  records <- fix_usb(
    Type = records[[1]]$Type,
    verbose = verbose,
    records = records
  )

  records <- collapse_records(
    records, label, verbose
  )

  # records <- interpolate_sensor_records(
  #   records, schema, verbose
  # )

  records <- post_process(records)

  records$Timestamp <- timestamp_recalc(
    records$Timestamp, tz,
    verbose, schema$Payload$samples, label
  )

  records

}
