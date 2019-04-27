create_zero_record <- function(
  Type = "26", Timestamp, info
) {

  Payload <- switch(
    Type, "26" = zero_payload_26(info)
  )

  list(
    Type = Type,
    Timestamp = Timestamp,
    Size = NULL,
    Payload = Payload,
    Checksum = "OK"
  )

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
