# rm(list = ls())
# load("example_data.RData")
# record_set <- record_headers[[5]]
# # record_set <- record_set[1:5, ]
# test_result <- process_record_set(
#   record_set, log, tz, info, give_timestamp,
#   parameters, schema, verbose
# )

process_record_set <- function(record_set, log, tz,
  info, give_timestamp, parameters = NULL, schema = NULL,
  verbose = FALSE, do_post_process = TRUE
) {
  
  ## Setup
  n_vals <- nrow(record_set)
  label <- RECORDS$Type[match(
    record_set$type[1],
    as.character(RECORDS$ID)
  )]
  
  if (verbose) cat(
    "\n  Parsing", label, "packet(s)"
  )
  
  records <- lapply(
    seq(n_vals),
    function(i) {
      
      if (verbose & (i != n_vals)) cat(
        "\r  Parsing", label, "packet(s)",
        "  .............",
        paste(
          c(round(i/n_vals * 100, 0), "%"),
          collapse = ""
        )
      )
      
      result <- read_record(
        record_set[i, ], log, tz,
        info, give_timestamp, parameters, schema,
        is_last_packet = i == n_vals
      )
      
      if (is.null(result$Payload)) return(NULL)
      return(result)
    }
  )
  
  records[sapply(records, is.null)] <- NULL
  records <- collapse_records(records, label = label)
  cat(
    "\r  Parsing", label, "packet(s)",
    "  .............", "100%"
  )

  if (do_post_process) records <- post_process(records)
  
  if (verbose) cat(
    "\r  Parsing", label, "packet(s)",
    "  ............. COMPLETE"
  )
  
  return(records)
  
}

collapse_records <- function(records, label) {
  switch(
    label,
    "METADATA" = generic_record_collapse(records, label),
    "PARAMETERS" = records[[1]], ## Max of one packet already verified
    "SENSOR_SCHEMA" = records[[1]], ## Max of one packet already verified
    "BATTERY" = generic_record_collapse(records, label),
    "EVENT" = generic_record_collapse(records, label),
    "TAG" = list(),
    "ACTIVITY" = list(),
    "HEART_RATE_BPM" = list(),
    "HEART_RATE_ANT" = list(),
    "HEART_RATE_BLE" = list(),
    "LUX" = list(),
    "CAPSENSE" = generic_record_collapse(records, label),
    "EPOCH" = list(),
    "EPOCH2" = list(),
    "EPOCH3" = list(),
    "EPOCH4" = list(),
    "SENSOR_DATA" = generic_record_collapse(records, label),
    "ACTIVITY2" = generic_record_collapse(records, label)
  )
}

generic_record_collapse <- function(records, label) {
  records <- lapply(
    records,
    function(x) data.frame(
      Timestamp = x$Timestamp,
      Type = label,
      Result = x$Payload,
      stringsAsFactors = FALSE
    )
  )
  records <- data.table::rbindlist(records, TRUE)
  # if (!type) records$Type <- NULL
  return(records)
}

post_process <- function(result) {
  
  names(result) <- gsub("^Result\\.", "", names(result))
  result <- data.frame(result, stringsAsFactors = FALSE)
  
  if (sum(grepl("Timestamp", names(result))) >1) {
    col_to_drop <- which(grepl("Timestamp", names(result)))[1]
    result <- result[ ,-c(col_to_drop)]
    if (sum(grepl("Timestamp", names(result))) == 1) {
      names(result) <- gsub("Timestamp.*", "Timestamp", names(result))
    }
  }
  
  return(result)
}