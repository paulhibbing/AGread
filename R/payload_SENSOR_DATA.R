#' Parse SENSOR_DATA packet
#'
#' This is primarily a wrapper for \code{\link{legacy_payload_parse_sensor_data_25C}}
#'
#' @param payload raw. The packet payload
#' @param parameters a PARAMETERS object
#' @param schema a SENSOR_SCHEMA object
#'
#' @keywords internal
#'
payload_parse_sensor_data_25 <- function(
  payload, parameters, schema
) {

  result <- legacy_payload_parse_sensor_data_25C(payload, schema)
  result <- lapply(
    result,
    function(x) if(!length(x)) return(NULL) else return(x)
  )
  result <- result[!sapply(result, is.null)]
  result <- do.call(data.frame, result)

  if ("Temperature" %in% names(result)) {
    result$Temperature <- result$Temperature + as.numeric(
      as.character(parameters$Payload$IMU_TEMP_OFFSET)
    )
  }
  if ("Discard" %in% names(result)) result$Discard <- NULL

  return(result)

}

#' Parse a column of sensor data
#'
#' @param payload raw. The payload to parse
#' @param indices vector giving the starting index of each record to be parsed
#' @param bytesInValue integer. Length of payload entry
#' @param is_signed logical. Is encoded value signed?
#' @param endianness character. Is endianness big or little?
#' @param scale_factor numeric. Scale factor for binary-to-unit conversions
#' @param label character. The column label
#'
#' @keywords internal
#'
get_sensor_column <- function(
  payload, indices, bytesInValue,
  is_signed, endianness, scale_factor, label
) {

  chunks <- sapply(
    indices,
    function(x) payload[x:(x + bytesInValue - 1)],
    simplify = FALSE
  )

  result <- lapply(
    chunks,
    function(chunk) readBin(
    chunk, "integer",
    bytesInValue, bytesInValue,
    is_signed, endianness
    )
  )

  result <- do.call(c, result)
  if (scale_factor != 0) {
    result <- result / scale_factor
  }

  is_temp <- grepl(
    "temperature", label, ignore.case = TRUE
  )

  if (is_temp) result <- result + 21
  ## Above: 21 Could be IMU_TEMP_OFFSET from PARAMETERS

  if (label != "") {
    return(
      stats::setNames(
        data.frame(
          result,
          stringsAsFactors = FALSE
        ),
        label
      )
    )
  }

  invisible()

}
