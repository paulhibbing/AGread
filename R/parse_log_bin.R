parse_log_bin <- function(
  log_file, n_records, info, tz, verbose = FALSE,
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
      log <- readBin(log_file, "raw", n_records)
    if (verbose) cat("  ............. COMPLETE")
  
  ## Get headers
    record_headers <- get_headers(
      log, n_records, tz, verbose = verbose
    )

    stopifnot(
      all(
        sum(record_headers$type == "21") <= 1,
        sum(record_headers$type == "24") <= 1
      )
    )
  
  ## Get parameters (if applicable)
    par_index <- which(record_headers$type == "21")
    if (!!length(par_index)) {
      parameters <- process_record_set(
        record_headers[par_index, ],
        log, tz, info, give_timestamp,
        verbose = verbose, do_post_process = FALSE
      )
      record_headers <- record_headers[-par_index, ]
    }
    
  ## Get schema (if applicable)
    schema_index <- which(record_headers$type == "24")
    if (!!length(schema_index)) {
      schema <- process_record_set(
        record_headers[schema_index, ],
        log, tz, info, give_timestamp,
        verbose = verbose, do_post_process = FALSE
      )
      record_headers <- record_headers[-schema_index, ]
    }
    
  ## Process the remaining packets
    record_headers <- sort_records(record_headers)
    record_headers <- select_records(record_headers, include)
    
    # save.image("example_data.RData")
    
    results <- lapply(
      record_headers,
      process_record_set,
      log = log, tz = tz, info = info,
      give_timestamp = give_timestamp,
      parameters = parameters, schema = schema,
      verbose = verbose, do_post_process = TRUE
    )

    return(results)
    
}

name_log <- function(log) {
  
  log_names <- sapply(
    log, function(x) x$Type[1]
  )
  
  log_names <- gsub("ACTIVITY2", "RAW", log_names)
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
