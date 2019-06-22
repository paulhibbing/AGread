#' Retrieve record headers from .gt3x binary data
#'
#' @param log raw. The data from log.bin
#' @inheritParams parse_log_bin
#'
#' @keywords internal
#'
get_headers <- function(log, tz = "UTC", verbose = FALSE) {

  if (verbose) cat("\n  Getting record headers")

  record_headers <- get_headersC(log)
  record_headers$index <- record_headers$index + 1
  record_headers$type  <- as.character(record_headers$type)
  record_headers$timestamp <- anytime::anytime(
    record_headers$timestamp, tz
  )

  if (verbose) cat("............... COMPLETE")

  return(record_headers)

}
