rm(list = ls())
load("example_data.RData")
record_set <- record_headers[[5]]
# record_set <- record_set[1:5, ]
test_result <- process_record_set(
  record_set, log, tz, info, give_timestamp,
  parameters, schema, verbose
)

process_record_set <- function(record_set, log, tz,
  info, give_timestamp, parameters = NULL, schema = NULL,
  verbose = FALSE, post_process = TRUE
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
  
  ## Initialize the result object
  result <- NULL
  
  ## Iterate through the packets and update the result object
  for (i in seq(n_vals)) {
    
    if (verbose) {
      # cat(
      #   "\r Parsing", i, "of", n_vals,
      #   label, "packet(s)"
      # )
      cat(
        "\r  Parsing", label, "packet(s)",
        "  .............",
        paste(
          round(i/n_vals * 100, 0),
          "%",
          collapse = ""
        )
      )
    }
    
    new_entry <- read_record(
      record_set[i, ], log, tz,
      info, give_timestamp, parameters, schema,
      is_last_packet = i == n_vals
    )
    
    if (!is.null(new_entry)) {
      result <- collapse_records(
        result, new_entry, label
      )
    }
    
  }
  
  if (post_process) result <- post_process(result)
  
  if (verbose) cat(
    "\r  Parsing", label, "packet(s)",
    "  ............. COMPLETE"
  )
  
  return(result)
  
}

collapse_records <- function(result, new_entry, label, payload) {
  switch(
    label,
    "METADATA" = generic_record_collapse(result, new_entry, label),
    "PARAMETERS" = new_entry, ## Max of one packet already verified
    "SENSOR_SCHEMA" = new_entry, ## Max of one packet already verified
    "BATTERY" = generic_record_collapse(result, new_entry, label),
    "EVENT" = generic_record_collapse(result, new_entry, label),
    "TAG" = list(),
    "ACTIVITY" = list(),
    "HEART_RATE_BPM" = list(),
    "HEART_RATE_ANT" = list(),
    "HEART_RATE_BLE" = list(),
    "LUX" = list(),
    "CAPSENSE" = generic_record_collapse(result, new_entry, label),
    "EPOCH" = list(),
    "EPOCH2" = list(),
    "EPOCH3" = list(),
    "EPOCH4" = list(),
    "SENSOR_DATA" = generic_record_collapse(
      result, new_entry, label, FALSE
    ),
    "ACTIVITY2" = generic_record_collapse(
      result, new_entry, label, FALSE
    )
  )
}

generic_record_collapse <- function(result, new_entry, label, type = TRUE) {
  new_entry <- data.frame(
    Timestamp = new_entry$Timestamp,
    Type = label,
    Result = new_entry$Payload,
    stringsAsFactors = FALSE
  )
  if (!type) new_entry$Type <- NULL
  data.table::rbindlist(list(result, new_entry), TRUE)
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