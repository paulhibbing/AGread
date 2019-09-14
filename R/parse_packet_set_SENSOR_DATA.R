#' @rdname parse_packet_set
#' @export
parse_packet_set.SENSOR_DATA <- function(
  set, log, tz = "UTC", verbose = FALSE,
  schema, ...
) {

  IMU <- parse_IMU_C(
    set, log, schema$sensorColumns, schema$id, verbose
  )

  if (verbose) cat(
    "\r  Calculating timestamps",
    "                           "
  )

    IMU <- lapply(IMU, function(x) {
      value <- as.POSIXct(x$Timestamp, tz)
      increments <- seq_along(value) - 1
      x$Timestamp <- value + (increments / length(value))
      x
    })

  if (verbose) cat(
    "\r  Merging packets       ",
    "                         "
  )

    IMU <- data.frame(
      data.table::rbindlist(IMU),
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    class(IMU) <- append(class(IMU), "IMU", 0)

    warning(paste(
      "This function is not complete --",
      "Still needs gap",
      "\n  checking/interpolation."
    ))
  # if (verbose) cat(
  #   "\r  Checking for gaps in the",
  #   "time series. Fixing if found."
  # )
  #
  # IMU <- check_gaps(IMU, info = info)
  #
  if (verbose) packet_print("cleanup", class(set)[1])

  IMU

}
