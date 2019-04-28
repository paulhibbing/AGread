create_zero_record <- function(
  Type = "26", Timestamp, info
) {

  Payload <- switch(
    Type,
    "25" = zero_payload_25(info),
    "26" = zero_payload_26(info)
  )

  list(
    Type = Type,
    Timestamp = Timestamp,
    Size = NULL,
    Payload = Payload,
    Checksum = "OK"
  )

}

zero_payload_25 <- function(schema) {

  n_rows <- schema$Payload$samples
  if (n_rows == 0) n_rows <- 100

  col_names <- schema$Payload$sensorColumns$label
  col_names <- col_names[col_names != ""]
  col_names <- gsub(" ", "_", col_names)

  payload <- matrix(
    0, n_rows, length(col_names)
  )

  payload <- stats::setNames(
    data.frame(
      payload,
      row.names = NULL
    ),
    col_names
  )

  payload$interpolate <- 0

  payload

}

zero_payload_26 <- function(info) {

  stats::setNames(
    data.frame(
      matrix(0, info$Sample_Rate, 3),
      row.names = NULL
    ),
    c(
      "Accelerometer_X", "Accelerometer_Y",
      "Accelerometer_Z"
    )
  )

}
