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

#' Parse primary accelerometer packets
#'
#' @param primary_records data frame with information about the primary
#'   accelerometer packets
#' @param log the gt3x raw data
#' @param info the result of \code{\link{parse_info_txt}}
#' @inheritParams read_gt3x
#'
#' @return
#' @export
#'
#' @examples
parse_primary_accelerometer <- function(
  primary_records, log, info, tz, verbose = FALSE
) {

  scale_factor <- get_primary_accel_scale(info)
  primary_records$timestamp <- as.character(
    primary_records$timestamp
  )

  RAW <- parse_primary_accelerometerC(
    primary_records, log,
    scale_factor, info$Sample_Rate, verbose
  )

  if (verbose) cat(
    "\r  Calculating timestamps",
    "                         "
  )
    RAW <- lapply(RAW, function(x) {
      value <- as.POSIXct(x$Timestamp, tz)
      increments <- seq_along(value) - 1
      x$Timestamp <- value + (increments / length(value))
      x
    })

    RAW <- data.frame(
      data.table::rbindlist(RAW),
      stringsAsFactors = FALSE,
      row.names = NULL
    )

  if (verbose) cat(
    "\r  Rounding accelerations",
    "                         "
  )
    accel_names <- paste(
      "Accelerometer", c("X", "Y", "Z"), sep = "_"
    )
    stopifnot(all(accel_names %in% names(RAW)))

    RAW[ ,accel_names] <- sapply(
      RAW[ ,accel_names], round, digits = 3
    )

  if (verbose) cat(
    "\r  Parsing", "ACTIVITY2", "packet(s)",
    "  ............. COMPLETE               ",
    "      "
  )
    RAW

}
