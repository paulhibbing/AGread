.accel_names <- c("Accelerometer_X", "Accelerometer_Y", "Accelerometer_Z")
.gyro_names <- c("Gyroscope_X", "Gyroscope_Y", "Gyroscope_Z")
.mag_names <- c("Magnetometer_X", "Magnetometer_Y", "Magnetometer_Z")
.packets <- c(
  "METADATA", "PARAMETERS", "SENSOR_SCHEMA", "BATTERY", "EVENT",
  "TAG", "ACTIVITY", "HEART_RATE_BPM", "HEART_RATE_ANT", "HEART_RATE_BLE",
  "LUX", "CAPSENSE", "EPOCH", "EPOCH2", "EPOCH3", "EPOCH4", "ACTIVITY2",
  "SENSOR_DATA"
)

#' Construct missing packet entries for ACTIVITY2 (RAW) data
#'
#' @param timestamps the packet timestamps
#' @param empty_value the value to assign missing accelerometer entries
#' @param info output of \code{\link{parse_info_txt}}
#' @param empty_frame optional data frame of values to pass for accelerometer
#'   axes (used for latching)
#'
#' @keywords internal
empty_raw <- function(
  timestamps, empty_value = NA, info, empty_frame = NULL
) {

  if (!length(timestamps)) {
    return(structure(
      list(
        Timestamp = structure(
          numeric(0),
          class = c("POSIXct", "POSIXt")
        ),
        Accelerometer_X = numeric(0),
        Accelerometer_Y = numeric(0),
        Accelerometer_Z = numeric(0)
      ),
      row.names = integer(0),
      class = "data.frame"
    ))
  }

  milliseconds <- seq(info$Sample_Rate) - 1
  milliseconds <- milliseconds / info$Sample_Rate

  missing_times <- sapply(
    timestamps, function(x) x + milliseconds,
    simplify = FALSE
  ) %>% {do.call(c, .)}

  if (is.null(empty_frame)) {

    missing_entries <- data.frame(
      Timestamp = missing_times,
      Accelerometer_X = empty_value,
      Accelerometer_Y = empty_value,
      Accelerometer_Z = empty_value
    )

  } else {

    missing_entries <- data.frame(
      Timestamp = missing_times,
      empty_frame, row.names = NULL
    )

  }

  missing_entries

}

#' Convert a tick value to a timestamp
#'
#' @param x the tick value
#' @param tz character. The timezone to use
#' @param ... further arguments passed to \code{as.POSIXct}
#'
#' @keywords internal
#'
tick_to_posix <- function(x, tz = "UTC", ...) {

  x <- as.numeric(as.character(x)) / 10000000

  as.POSIXct(x, tz, origin = "0001-01-01", ...)

}

#' Calculate an integer from a binary sequence
#'
#' @param value_bin The binary sequence
#'
#' @keywords internal
#'
bin_int <- function(value_bin) {
  sum(2 ^ (length(value_bin) - which(value_bin)))
}

#' Convert a raw elements to a binary sequence
#'
#' Essentially a wrapper for \code{\link[binaryLogic]{as.binary}}
#'
#' @param value raw. The elements to convert
#' @param n integer. Desired length of output for each element
#'
#' @keywords internal
#'
AG_binary <- function(value, n = 8) {
  binaryLogic::as.binary(
    unlist(binaryLogic::as.binary(rev(value), n = n)),
    logic = TRUE
  )
}

#' Evaluate the value portion of a PARAMETERS record
#'
#' @param type character. The record type
#' @param value raw. The record to evaluate
#'
#' @keywords internal
#'
get_value <- function(type, value) {
  switch(
    type,
    "float" = get_float_value(value),
    "int" = readBin(value, "integer", 4, 4)
  )
}

#' Retrieve the float value for appropriate PARAMETERS records
#'
#' @param value raw. The record to convert
#'
#' @keywords internal
#'
get_float_value <- function(value) {
  n_bytes <- length(value)
  n_bits <- n_bytes * 8
  exponent <- get_exponent(value, n_bytes)
  significand <- get_significand(value, n_bits, n_bytes)
  significand * (2^exponent)
}

#' @rdname get_float_value
#' @keywords internal
get_exponent <- function(value, n_bytes) {

  x <- value[n_bytes]
  binx <- AG_binary(x, n = 8)
  is_negative <- binx[1] == binaryLogic::as.binary(1)

  exponent <- as.integer(binx)
  if (is_negative) {
    # magnitude <- binaryLogic::binAdd(
    #   !binx, binaryLogic::as.binary(1)
    # )
    exponent <- exponent * -1
  }

  return(as.double(exponent))

}

#' @rdname get_float_value
#' @keywords internal
get_significand <- function(value, n_bits, n_bytes) {

  FLOAT_MAXIMUM <- 2^((n_bits - 8) - 1)
  x <- value[rev(seq(value))[-1]]

  binx <- AG_binary(rev(x), n = 8)
  is_negative <- binx[1] == binaryLogic::as.binary(1)

  significand <- as.integer(binx)
  if (is_negative) {
    # magnitude <- binaryLogic::binAdd(
    #   !binx, binaryLogic::as.binary(1)
    # )
    significand <- significand * -1
  }

  as.double(significand) / FLOAT_MAXIMUM

}

#' Final formatting for a PARAMETERS result
#'
#' @param key the key entry to use as a shell
#' @param value the value to append
#'
#' @keywords internal
#'
update_key <- function(key, value) {
  key <- key[1, ]
  key$Range <- NA
  key$value <- value
  return(key)
}

#' Collapse a character vector for tidy printing
#'
#' @param caps the vector to collapse
#'
#' @keywords internal
#'
capability_collapse <- function(caps) {
  cap_length <- length(caps)
  if (cap_length == 1) return (caps)
  last_cap <- caps[length(caps)]
  caps <- paste(caps[-length(caps)], collapse = ", ")
  caps <- paste(c(caps, ", and ", last_cap), collapse = "")
  if (cap_length == 2) caps <- gsub(", and", " and", caps)
  return(caps)
}

#' Retrieve the primary accelerometer scale factor
#'
#' @param info the contents of \code{info.txt}
#'
#' @return the scale factor, as integer
#' @keywords internal
get_primary_accel_scale <- function(info) {

  scale_factor <- switch(
    substring(info$Serial_Number, 1, 3),
    "NEO" = 341,
    "CLE" = 341,
    "MOS" = 256
  )

  if ("Acceleration_Scale" %in% names(info)) {
    scale_factor <- info$Acceleration_Scale
  }

  stopifnot(all.equal(
    scale_factor, as.integer(scale_factor),
    scale = 1, tolerance = 0.0
  ))

  as.integer(scale_factor)

}
