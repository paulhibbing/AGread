parse_info_txt <- function(info, verbose, ...) {
  
  if (verbose) cat("\n  Parsing info.txt")
  
  # Read text file and assemble data frame
  meta <- readLines(info)
  meta <- strsplit(meta, ": ")
  meta_names <- unlist(
    lapply(meta, function(x) x[1])
  )
  meta_names <- gsub(" ", "_", meta_names)
  meta <- data.frame(
    t(unlist(lapply(meta, function(x) x[2]))),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  names(meta) <- meta_names
  
  # Format data frame
  num_vars <- c(
    "Battery_Voltage", "Sample_Rate", "Board_Revision",
    "Unexpected_Resets", "Acceleration_Scale",
    "Acceleration_Min", "Acceleration_Max"
  )
  
  stopifnot(all(num_vars %in% names(meta)))
  
  for (i in num_vars) {
    meta[ ,i] <- as.numeric(
      as.character(meta[ ,i])
    )
  }
  
  tick_vars <- c(
    "Start_Date", "Stop_Date",
    "Last_Sample_Time", "Download_Date"
  )
  
  stopifnot(all(tick_vars %in% names(meta)))
  
  for (j in tick_vars) {
    meta[ ,j] <- tick_to_posix(
      meta[ ,j]
    )
  }
  
  meta$Download_Date <- strftime(
    meta$Download_Date,
    "%m/%d/%Y"
  )

  if (verbose) cat("  ............. COMPLETE")
      
  return(meta)
  
}
