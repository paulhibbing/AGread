#' Parse SENSOR_DATA packet
#'
#' @inheritParams payload_parse
#' @param record_header data frame with information about the record in question
#'
#' @keywords internal
#'
payload_parse_sensor_data_25 <- function(
  payload, info, parameters, schema, record_header
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

    if (schema$Payload$samples == 0) {
      schema$Payload$samples <- 100
    }

  ## Column-wise assembly
    columns <- split(
      schema$Payload$sensorColumns,
      seq(nrow(schema$Payload$sensorColumns))
    )

    result <- lapply(
      seq(schema$Payload$samples),
      function(i) {

        BITS_PER_BYTE <- get(
          "BITS_PER_BYTE", envir = parent.frame(2)
        )

        offsets <- ((i - 1) * BYTES_PER_RECORD) +
          BYTE_OFFSETS + orig_offset

        final_index <- offsets[length(offsets)] +
          (schema$Payload$sensorColumns$size[
            length(schema$Payload$sensorColumns$size)
            ] / BITS_PER_BYTE)

        sens_row <- data.table::rbindlist(
          mapply(
            get_sensor_row,
            column = columns,
            offset = offsets,
            MoreArgs = list(
              payload = payload,
              BITS_PER_BYTE = BITS_PER_BYTE
            ),
            SIMPLIFY = FALSE
          )
        )

        data.frame(
          reshape2::dcast(
            sens_row, .~label, value.var = "result"
          ),
          row.names = NULL, stringsAsFactors = FALSE
        )[ ,-1]

      }
    )

    result <- data.table::rbindlist(result, TRUE)
    row_ms <- seq(nrow(result)) - 1
    result$Timestamp <- firstSample + (row_ms / schema$Payload$samples)

    return(result)

}

#' Parse a row of sensor data
#'
#' @param payload raw. The payload to parse
#' @param column data frame. Information about the column in which the result
#'   will be placed
#' @param offset integer. The starting index for parsing
#' @param BITS_PER_BYTE integer. The number of bits per byte
#'
#' @keywords internal
#'
get_sensor_row <- function(
  payload, column, offset, BITS_PER_BYTE
) {

  bytesInValue <- column$size/BITS_PER_BYTE
  stopifnot(bytesInValue %in% 1:2)

  chunk <- payload[offset:sum(offset, bytesInValue - 1)]

  endianness <- "little"
  # if (column$is_big_endian) endianness <- "big"
  if (column$is_big_endian) endianness <- "big"

  result <- readBin(
    chunk, "integer",
    bytesInValue, bytesInValue,
    column$is_signed, endianness
  )

  if (column$scale_factor != 0) {
    result <- result / column$scale_factor
  }

  is_temp <- grepl(
    "temperature", column$label, ignore.case = TRUE
  )

  if (is_temp) result <- result + 21

  if (column$label != "") {
    return(
      data.frame(
      label = gsub(" ", "_", column$label),
      result = result,
      stringsAsFactors = FALSE
      )
    )
  }

  invisible()

}
