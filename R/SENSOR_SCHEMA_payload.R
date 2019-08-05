#' Parse metadata from a SENSOR_SCHEMA packet
#'
#' @inheritParams payload_parse
#'
#' @keywords internal
#'
schema_meta <- function(payload) {

  id <- payload[1:2]
  id <- readBin(
    id, "integer", 2, 2, TRUE, "little"
  )

  columns <- payload[3:4]
  columns <- readBin(
    columns, "integer", 2, 2, TRUE, "little"
  )

  samples <- payload[5:6]
  samples <- readBin(
    samples, "integer", 2, 2, TRUE, "little"
  )

  return(
    list(
      id = id, columns = columns,
      samples = samples, sensorColumns = data.frame()
    )
  )
}

#' Parse metadata from a SENSOR_SCHEMA packet
#'
#' @inheritParams payload_parse
#'
#' @keywords internal
#'
payload_parse_sensor_schema_24 <- function(payload, info) {

  BYTES_PER_COLUMN <- 23

  # payload <- rev(payload)
  schema  <- schema_meta(payload)

  stopifnot(
    all.equal(
      (schema$columns * BYTES_PER_COLUMN) + 6,
      length(payload)
    )
  )

  for (i in seq(schema$columns) - 1) {
    # i <- 0
    startingOffset <- 7 + (BYTES_PER_COLUMN * i)
    # indices <- seq(
    #   startingOffset,
    #   startingOffset + BYTES_PER_COLUMN - 1
    # )
    columnFlags <- payload[startingOffset]

    bigEndian <- bin_int(
      AG_binary(columnFlags[1]) &
        AG_binary(bitwShiftL(1, 0))
      ) != 0
    signed <- bin_int(
      AG_binary(columnFlags[1]) &
        AG_binary(bitwShiftL(1, 1))
    ) != 0

    columnOffset <- payload[startingOffset + 1]
    columnSize <- payload[startingOffset + 2]
    value <- payload[startingOffset + 3:6]

    columnScaleFactor <- AG_round(
      get_float_value(value)
    )

    columnLabel <- rawToChar(payload[startingOffset + 7:22])
    columnLabel <- gsub("^ *", "", gsub(" *$", "", columnLabel))
    columnLabel <- gsub(" ", "_", columnLabel)
    columnLabel <- ifelse(columnLabel == "", "Discard", columnLabel)

    new_column <- data.frame(
      is_big_endian = bigEndian,
      is_signed = signed,
      offset = readBin(
        columnOffset, "integer", 1, 1, FALSE, "little"
      ),
      offset_bytes = readBin(
        columnOffset, "integer", 1, 1, FALSE, "little"
      ) / 8,
      size = readBin(
        columnSize, "integer", 1, 1, FALSE, "little"
      ),
      n_bytes = readBin(
        columnSize, "integer", 1, 1, FALSE, "little"
      ) / 8,
      scale_factor = columnScaleFactor,
      label = columnLabel,
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    schema$sensorColumns <- rbind(schema$sensorColumns, new_column)

  }

  if (schema$samples == 0) {
    schema$samples <- 100
  }

  return(schema)

}
