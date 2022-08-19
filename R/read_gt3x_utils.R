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

#' Switch character packet inclusion vector to an integer vector for Rcpp dev
#' parser
#'
#' @inheritParams read_gt3x
#'
#' @keywords internal
cpp_include <- function(include) {
  match(include, .packets) %>%
  .numbers[.]
}

#' Check for packets with failed checksum
#'
#' @inheritParams get_parameters
#'
#' @keywords internal
check_packets <- function(packets) {

  failed <- sapply(packets, is.null)

  if (!any(failed)) {

    return(packets)

  } else {

    message(
      "\nSkipping ", sum(failed),
      " packet(s) for which checksum failed"
    )

    packets[!failed]

  }


}

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
