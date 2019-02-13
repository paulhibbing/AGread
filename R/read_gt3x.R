# source("Helpers/setup.R")
read_gt3x <- function(
  file, tz = "UTC", verbose = FALSE,
  give_timestamp = TRUE,
  include = c("METADATA", "PARAMETERS", "SENSOR_SCHEMA", "BATTERY", "EVENT", 
  "TAG", "ACTIVITY", "HEART_RATE_BPM", "HEART_RATE_ANT", "HEART_RATE_BLE", 
  "LUX", "CAPSENSE", "EPOCH", "EPOCH2", "EPOCH3", "EPOCH4", "ACTIVITY2", 
  "SENSOR_DATA")
) {

  timer <- proc.time()  
  if (verbose) cat("\nProcessing", basename(file), "\n")
  
  #1) Verify .gt3x file is a zip file
  file_3x <- try(
    utils::unzip(file, list = TRUE),
    TRUE
  )
  
  if (class(file_3x) == "try-error") {
    stop(paste(
      deparse(substitute(file)),
      "is not a valid gt3x file."
    ))
  } else {
    row.names(file_3x) <- file_3x$Name
  }
  
  #2) Verify .gt3x file has log.bin file
  #3) Verify .gt3x file has info.txt file
  stopifnot(all(c("info.txt", "log.bin") %in% file_3x$Name))
  
  #4) Extract info.txt
  info_con <- unz(file, "info.txt")
  
  #5) Parse and save the sample rate from the info.txt file (it's stored in Hz)
  #6) Parse and save the start date from the info.txt file (it's stored in .NET
  #Ticks)
  info <- parse_info_txt(info_con, verbose)
  close(info_con)

  #7) Extract log.bin
  #8) Parse log.bin
  # n_records <- file_3x["log.bin", "Length"]
  log_file  <- utils::unzip(file, "log.bin", exdir = tempdir())
  log  <- parse_log_bin(
    log_file, file_3x["log.bin", "Length"], info, tz,
    verbose, give_timestamp, include
  )
  
  if (verbose) cat(
    "\n\nProcessing complete. Elapsed time",
    AGread::get_duration(timer),
    "minutes."
  )  
  
  return(name_log(log))
  
}
