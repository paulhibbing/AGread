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
  give_timestamp = TRUE, include = c(
    "METADATA", "PARAMETERS", "SENSOR_SCHEMA", "BATTERY", "EVENT",
    "TAG", "ACTIVITY", "HEART_RATE_BPM", "HEART_RATE_ANT", "HEART_RATE_BLE",
    "LUX", "CAPSENSE", "EPOCH", "EPOCH2", "EPOCH3", "EPOCH4", "ACTIVITY2",
    "SENSOR_DATA"
  )
) {

  ## Set up information about which packets to include
  choices <- c(
    "METADATA", "PARAMETERS", "SENSOR_SCHEMA", "BATTERY", "EVENT",
    "TAG", "ACTIVITY", "HEART_RATE_BPM", "HEART_RATE_ANT", "HEART_RATE_BLE",
    "LUX", "CAPSENSE", "EPOCH", "EPOCH2", "EPOCH3", "EPOCH4", "ACTIVITY2",
    "SENSOR_DATA"
  )
  stopifnot(all(include %in% choices))
  include <- match.arg(include, c(choices, "Error"), TRUE)

  if (verbose) {
    CHOICES <- split(include, cumsum(seq(include)%%4 == 1))
    CHOICES <- lapply(CHOICES, function(x) paste(x, collapse = ", "))
    cat("\n\n  Will parse the following packet types, if available:\n")
    lapply(CHOICES, function(x) cat("   ", x, "\n"))
  }

  ## Read the bin file
    if (verbose) cat("\n  Reading log.bin")
      log <- readBin(log_file, "raw", file_3x_len)
    if (verbose) cat("  ............. COMPLETE")

  ## Get headers
    record_headers <- get_headers(log, tz, verbose)

    stopifnot(
      all(
        sum(record_headers$type == "21") <= 1,
        sum(record_headers$type == "24") <= 1
      )
    )

  ## Get parameters (if applicable)
    par_info <- special_header(
      record_headers, "21", log, tz,
      info, give_timestamp, verbose, FALSE
    )
    if (!is.null(par_info)) {
      parameters <- par_info$result
      record_headers <- record_headers[-par_info$index, ]
    }

  ## Get schema (if applicable)
    schema_info <- special_header(
      record_headers, "24", log, tz,
      info, give_timestamp, verbose, FALSE
    )
    if (!is.null(schema_info)) {
      schema <- schema_info$result
      record_headers <- record_headers[-schema_info$index, ]
    }

  ## Arrange the remaining packets
    record_headers <- sort_records(record_headers)
    record_headers <- select_records(record_headers, include)
    types <- sapply(record_headers, function(x) x$type[1])

    # save.image("data-raw/example_data.RData")

  ## If applicable, deal with ACTIVITY2 packets in C++
    if ("26" %in% types) {
      if (verbose) cat("\n")
      index <- which(types == "26")
      primary_records <- record_headers[[index]]
      record_headers <- record_headers[-index]
      RAW <- parse_primary_accelerometer(
        primary_records, log, info, tz, verbose
      )
    }

  ## Now process the remaining packets
    results <- lapply(
      record_headers,
      process_record_set,
      log = log, tz = tz, info = info,
      give_timestamp = give_timestamp,
      parameters = parameters, schema = schema,
      verbose = verbose, do_post_process = TRUE
    )

    if(all("PARAMETERS" %in% include, exists("parameters"))) {
      results$PARAMETERS <- parameters
    }
    if(all("SENSOR_SCHEMA" %in% include, exists("schema"))) {
      results$SENSOR_SCHEMA <- schema
    }
    if ("26" %in% types) {
      results$RAW <- RAW
    }

    return(results)

}

#' Lightly format a processed log
#'
#' @param log the log to process
#'
#' @keywords internal
#'
name_log <- function(log) {

  log_names <- sapply(
    log, function(x) {
      type_name <- x$Type[1]
      if (is.null(type_name)) return(NA)
      type_name
    }
  )

  log_names <- unname(ifelse(
    is.na(log_names), names(log_names), log_names
  ))

  log_names <- gsub("^21$", "PARAMETERS", log_names)
  log_names <- gsub("^24$", "SENSOR_SCHEMA", log_names)
  log_names <- gsub("SENSOR_DATA", "IMU", log_names)

  log <- stats::setNames(
    log, log_names
  )

  log <- lapply(
    log,
    function(x) {
      x$Type <- NULL
      return(x)
    }
  )

  if ("IMU" %in% names(log)) {
    IMU <- log$IMU
    desired_order <- c(
      "Timestamp",
      "Accelerometer_X", "Accelerometer_Y", "Accelerometer_Z",
      "Temperature",
      "Gyroscope_X", "Gyroscope_Y", "Gyroscope_Z",
      "Magnetometer_X", "Magnetometer_Y", "Magnetometer_Z"
    )
    ordered_names <- desired_order[desired_order %in% names(IMU)]
    log$IMU <- IMU[ ,ordered_names]
  }

  return(log)

}
