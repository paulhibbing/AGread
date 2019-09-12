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

#' Function to round away from zero
#'
#' @param x the value to round
#' @param digits digits to round to
#'
#' @keywords internal
#'
AG_round <- function(x, digits = 6) {
  # x <- c(7.5555555, -7.5555555)
  # digits <- 6

  signs <- sign(x)

  DescTools::RoundTo(
    abs(x), 5 / (10 ^ digits), "ceiling"
  ) * signs

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
#' @inheritParams parse_primary_accelerometer
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
