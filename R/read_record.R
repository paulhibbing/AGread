# View(do.call(rbind, record_headers))
# record_header <- record_headers[[23]]
# ... further arguments passed to payload_parse
read_record <- function(
  record_header, log, tz = "UTC", info, give_timestamp = TRUE,
  parameters = NULL, schema = NULL, ...
) {
  
  log_indices <- seq(
    record_header$index,
    record_header$index + 8 + record_header$payload_size
  )
  
  record <- log[log_indices]
  
  payload_raw <- record[9:(length(record) - 1)]
  stopifnot(length(payload_raw) == record_header$payload_size)
  
  payload <- payload_parse(
    record_header$type, payload_raw, info, tz,
    parameters, schema, record_header, ...
  )
  
  checksum <- checksum_calculate(
    record = record,
    final_index = log_indices[length(log_indices)]
  )
  checksum <- if (is.null(checksum)) "OK" else "ERROR"
  
  value <- list(
      Type = record_header$type,
      Timestamp = record_header$timestamp,
      Size = record_header$size,
      Payload = payload,
      Checksum = checksum
  )
  
  if (!give_timestamp) value$Timestamp <- NULL
  
  return(value)
  
}
