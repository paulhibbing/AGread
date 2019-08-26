#' Validate the values provided for the \code{input} argument in
#' \code{\link{read_gt3x}}
#'
#' @inheritParams read_gt3x
#' @param choices character. The packet types to choose from.
#'
#' @keywords internal
validate_include <- function(
  include,
  verbose = FALSE,
  choices = c(
    "METADATA", "PARAMETERS", "SENSOR_SCHEMA",
    "BATTERY", "EVENT", "TAG", "ACTIVITY",
    "HEART_RATE_BPM", "HEART_RATE_ANT",
    "HEART_RATE_BLE", "LUX", "CAPSENSE",
    "EPOCH", "EPOCH2", "EPOCH3", "EPOCH4",
    "ACTIVITY2", "SENSOR_DATA"
  )
) {

  stopifnot(all(include %in% choices))
  include <- match.arg(include, c(choices, "Error"), TRUE)

  if (verbose) {
    CHOICES <- split(include, cumsum(seq(include)%%4 == 1))
    CHOICES <- lapply(CHOICES, function(x) paste(x, collapse = ", "))
    cat("\n\n  Will parse the following packet types, if available:\n")
    lapply(CHOICES, function(x) cat("   ", x, "\n"))
  }

  include

}

#' Print packet-parsing information to console
#'
#' If \code{verbose} has been set to \code{TRUE} in the parent function, this
#' function will be invoked to construct the message and print it.
#'
#' @param type character. The message type
#' @param label character. The packet type
#' @param i numeric. Proportion to print as percentage for progress updates
#'
#' @keywords internal
packet_print <- function(
  type = c("startup", "progress", "cleanup"), label, i
) {

  switch(

    match.arg(type),

    "startup" = cat(
      "\n  Parsing", label, "packet(s)"
    ),

    "progress" =   cat(
      "\r  Parsing", label, "packet(s)",
      "  .............",
      paste(
        c(round(i * 100, 0), "%"),
        collapse = ""
      )
    ),

    "cleanup" = cat(
      "\r  Parsing", label, "packet(s)",
      "  ............. COMPLETE               ",
      "      "
    )

  )

}

#' Retrieve payload data for a single packet
#'
#' This function includes a call to \code{\link{checksumC}} and will break if
#' the result is not as expected
#'
#' @param record_header data frame containing information about the packet
#'   indices etc.
#' @param log raw. The data from log.bin
#'
#' @keywords internal
setup_payload <- function(record_header, log) {

  log_indices <- seq(
    record_header$index,
    record_header$index + 8 + record_header$payload_size
  )

  record <- log[log_indices]

  payload <- record[9:(length(record) - 1)]
  stopifnot(length(payload) == record_header$payload_size)

  checksumC(
    log, log_indices[1], log_indices[length(log_indices)]
  )

  payload

}
