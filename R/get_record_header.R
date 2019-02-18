#' Retrieve record headers from .gt3x binary data
#'
#' @inheritParams read_record
#' @param n_records integer. The length of log.bin
#' @param verbose logical. Print updates to console?
#'
#' @keywords internal
#'
get_headers <- function(log, n_records, tz = "UTC", verbose = FALSE) {

  if (verbose) cat("\n  Getting record headers")

  record_headers <- data.frame(
    index = 1, type = NA, timestamp = as.POSIXct(NA, tz), payload_size = NA,
    stringsAsFactors = FALSE
  )

  repeat {

    record_index <- nrow(record_headers)
    log_index <- record_headers$index[record_index]
    stopifnot(log[log_index] == "1e")

    record_headers$type[record_index] <- as.character(
      readBin(log[log_index + 1], "integer", 1, 1, FALSE)
    )

    record_headers$timestamp[record_index] <- as.character(anytime::anytime(
      readBin(log[log_index + 2:5], "integer", 4, 4),
      tz
    ))

    record_headers$payload_size[record_index] <- readBin(
      log[log_index + 6:7], "integer", 2, 2, FALSE
    )

    next_log_index <- record_headers$index[record_index] +
      record_headers$payload_size[record_index] + 9

    if (next_log_index > n_records) {
      break
    }

    record_headers[record_index + 1, ] <- NA
    record_headers[record_index + 1, "index"] <- next_log_index

  }

  if (verbose) cat("............... COMPLETE")

  return(record_headers)

}
