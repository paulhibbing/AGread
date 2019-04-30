#' Parse SENSOR_DATA packet
#'
#' @inheritParams payload_parse
#' @param record_header data frame with information about the record in question
#'
#' @keywords internal
#'
payload_parse_sensor_data_25 <- function(
  payload, parameters, schema, record_header
) {

  ## Setup
    id <- readBin(
      payload[1:2], "integer", 2, 2, TRUE, "little"
    )
    stopifnot(id == schema$Payload$id)

    BITS_PER_BYTE <- 8

    BYTES_PER_RECORD <- sum(
      schema$Payload$sensorColumns$size
    ) / BITS_PER_BYTE

    BYTE_OFFSETS <- schema$Payload$sensorColumns$offset /
      BITS_PER_BYTE

    orig_offset <- 3

    firstSample <- record_header$timestamp

  ## Column-wise starting indices for each record
    all_indices <- lapply(
      seq(schema$Payload$samples) - 1,
      function(i) ((i) * BYTES_PER_RECORD) +
        BYTE_OFFSETS + orig_offset
    )
    all_indices <- do.call(rbind, all_indices)
    # all_indices <- row_test(
    #   all_indices, payload, schema$Payload$samples
    # )

    if (nrow(all_indices) == 1) stop(
      "USB connection detected."
    )

    stopifnot(all(
      nrow(all_indices) == schema$Payload$samples,
      all_indices[1,1] == orig_offset,
      ncol(all_indices) == schema$Payload$columns
    ))

    result <- lapply(
      seq(nrow(schema$Payload$sensorColumns)),
      function(i) {
        column <- schema$Payload$sensorColumns[i, ]

        bytesInValue <- column$size/BITS_PER_BYTE
        stopifnot(bytesInValue %in% 1:2)

        is_signed <- column$is_signed

        endianness <- "little"
        if (column$is_big_endian) endianness <- "big"

        scale_factor <- column$scale_factor
        label <- gsub(" ", "_", column$label)
        indices <- all_indices[ ,i]

        get_sensor_column(
          payload, indices, bytesInValue,
          is_signed, endianness, scale_factor, label
        )
      }
    )

    result[sapply(result, is.null)] <- NULL
    result <- do.call(cbind, result)

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
