#' High-level function to read a packet of data from a gt3x file
#'
#' @param record_header data frame containing information about the packet
#'   indices etc.
#' @param log raw. The data from log.bin
#' @param tz character. The timezone to use
#' @param info result of \code{\link{parse_info_txt}}
#' @param give_timestamp logical. Include timestamp in output?
#' @param parameters result of parsing the PARAMETERS packet
#' @param schema result of parsing the SENSOR_SCHEMA packet
#' @param ... further arguments passed to \code{\link{payload_parse}}
#'
#' @keywords internal
#'
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
