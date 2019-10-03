#' Parse the log component of a gt3x file
#'
#' @param log_file character. Path to the log.bin file
#' @param file_3x_len length of log.bin, obtained from the \code{file_3x} object
#'   from the parent function \code{\link{read_gt3x}}
#' @param info result of \code{\link{parse_info_txt}}
#' @inheritParams read_gt3x
#'
#' @keywords internal
#'
parse_log_bin <- function(
  log_file, file_3x_len, info, tz = "UTC", verbose = FALSE,
  include = c(
    "METADATA", "PARAMETERS", "SENSOR_SCHEMA", "BATTERY", "EVENT",
    "TAG", "ACTIVITY", "HEART_RATE_BPM", "HEART_RATE_ANT", "HEART_RATE_BLE",
    "LUX", "CAPSENSE", "EPOCH", "EPOCH2", "EPOCH3", "EPOCH4", "ACTIVITY2",
    "SENSOR_DATA"
  )
) {

  ## Validate the input to `include`

    include <- validate_include(include, verbose)

  ## Read the bin file

    if (verbose) cat("\n  Reading log.bin")
    log <- readBin(log_file, "raw", file_3x_len)
    if (verbose) cat("  ............. COMPLETE")

  ## Get headers

    record_headers <- get_headers(log, tz, verbose)
    record_headers <- sort_records(record_headers)
    record_headers <- select_records(record_headers, include)

  ## Get parameters (if applicable)

    if ("PARAMETERS" %in% names(record_headers)) {
      parameters <- parse_packet_set(
        record_headers$PARAMETERS, log,
        tz, verbose
      )
      record_headers$PARAMETERS <- NULL
    } else {
      parameters <- NULL
    }

  ## Get schema (if applicable)

    if ("SENSOR_SCHEMA" %in% names(record_headers)) {
      schema <- parse_packet_set(
        record_headers$SENSOR_SCHEMA, log,
        tz, verbose
      )
      record_headers$SENSOR_SCHEMA <- NULL
    } else {
      schema <- NULL
    }

  ## Get events (if applicable)

    if (!"EVENT" %in% names(record_headers)) {
      events <- parse_packet_set(
        structure(list(), class = "EVENT"),
        log, tz, verbose
      )
    } else {
      events <- parse_packet_set(
        record_headers$EVENT,
        log, tz, verbose, info = info
      )
    }
    record_headers$EVENT <- NULL

  ## Now process the remaining packets

    results <- lapply(
      record_headers,
      parse_packet_set,
      log = log, tz = tz,
      verbose = verbose, info = info,
      parameters = parameters, schema = schema,
      events = events
    )

    if(all("PARAMETERS" %in% include, exists("parameters"))) {
      results$PARAMETERS <- parameters
    }
    if(all("SENSOR_SCHEMA" %in% include, exists("schema"))) {
      results$SENSOR_SCHEMA <- schema
    }
    if(all("EVENT" %in% include, exists("events"))) {
      results$EVENT <- events
    }

    new_names <- sapply(results, function(x) class(x)[1])
    new_names <- unname(
      ifelse(new_names == "NULL", names(new_names), new_names)
    )

    if (verbose) cat("\n")
    stats::setNames(results, new_names)

}
