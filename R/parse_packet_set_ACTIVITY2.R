#' @rdname parse_packet_set
#' @param info the result of \code{\link{parse_info_txt}}
#' @export
parse_packet_set.ACTIVITY2 <- function(
  set, log, tz = "UTC", verbose = FALSE,
  info, ...
) {

  scale_factor <- get_primary_accel_scale(info)
  set$timestamp <- as.character(set$timestamp)

  RAW <- parse_primary_accelerometerC(
    set, log, scale_factor,
    info$Sample_Rate, verbose
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

  if (verbose) cat(
    "\r  Merging packets       ",
    "                         "
  )

    RAW <- data.frame(
      data.table::rbindlist(RAW),
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    class(RAW) <- append(class(RAW), "RAW", 0)

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
    "\r  Checking for gaps in the",
    "time series. Fixing if found."
  )

    RAW <- check_gaps(RAW, info = info)

  if (verbose) packet_print("cleanup", class(set)[1])

    RAW

}
