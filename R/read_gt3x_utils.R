cpp_include <- function(include) {
  match(include, .packets) %>%
  .numbers[.]
}


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


tick_to_posix <- function(x, tz = "UTC", ...) {

  x <- as.numeric(as.character(x)) / 10000000

  as.POSIXct(x, tz, origin = "0001-01-01", ...)

}


bin_int <- function(value_bin) {
  sum(2 ^ (length(value_bin) - which(value_bin)))
}


AG_binary <- function(value, n = 8) {
  stop("`AG_binary` is defunct because of dependency on binaryLogic")
}


get_value <- function(type, value) {
  switch(
    type,
    "float" = get_float_value(value),
    "int" = readBin(value, "integer", 4, 4)
  )
}


get_float_value <- function(value) {
  n_bytes <- length(value)
  n_bits <- n_bytes * 8
  exponent <- get_exponent(value, n_bytes)
  significand <- get_significand(value, n_bits, n_bytes)
  significand * (2^exponent)
}


get_exponent <- function(value, n_bytes) {

  stop("`get_exponent` is defunct due to dependency on binaryLogic")

}


get_significand <- function(value, n_bits, n_bytes) {

  stop("`get_significand` is defunct due to dependency on binaryLogic")

}


update_key <- function(key, value) {
  key <- key[1, ]
  key$Range <- NA
  key$value <- value
  return(key)
}


capability_collapse <- function(caps) {
  cap_length <- length(caps)
  if (cap_length == 1) return (caps)
  last_cap <- caps[length(caps)]
  caps <- paste(caps[-length(caps)], collapse = ", ")
  caps <- paste(c(caps, ", and ", last_cap), collapse = "")
  if (cap_length == 2) caps <- gsub(", and", " and", caps)
  return(caps)
}


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
