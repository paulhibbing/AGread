#' Parse BATTERY packet
#'
#' @inheritParams payload_parse
#'
#' @keywords internal
#'
payload_parse_battery_2 <- function(payload, info) {
  paste(
    "BATTERY VOLTAGE:",
    readBin(payload, "integer", 2, 2, FALSE) / 1000
  )
}

#' Parse EVENT packet
#'
#' @inheritParams payload_parse
#'
#' @keywords internal
#'
payload_parse_event_3 <- function(payload, info) {
  return(list(
    payload = paste(as.character(payload), collapse = " "),
    description = "ActiGraph internal use. No further documentation."
  ))
}

#' Parse METADATA packet
#'
#' @inheritParams payload_parse
#'
#' @keywords internal
#'
payload_parse_infodata_6 <- function(payload, info) {
  rawToChar(payload)
}

#' Parse CAPSENSE packet
#'
#' @inheritParams payload_parse
#'
#' @keywords internal
#'
payload_parse_capsense_13 <- function(payload, info) {

  stopifnot(length(payload) == 6)

  signal <- readBin(payload[1:2], "integer", 2, 2, FALSE)
  reference <- readBin(payload[3:4], "integer", 2, 2, FALSE)
  state <- readBin(payload[5], "integer", 1, 1, FALSE)
  if (state == 0) state <- "Not Worn" else state <- "Worn"
  bursts <- readBin(payload[6], "integer", 1, 1, FALSE)

  data.frame(
    signal = signal,
    reference = reference,
    state = state,
    bursts = bursts,
    stringsAsFactors = FALSE
  )

}

#' Parse ACTIVITY2 packet
#'
#' @inheritParams payload_parse
#'
#' @keywords internal
#'
payload_parse_activity2_26 <- function(
  payload, info, scale_factor, is_last_packet = FALSE
) {

  if (length(payload) != 1) {

    test_pass <- all(
      length(payload) %% 3 == 0,
      length(payload) %% 2 == 0
    )
    if (is_last_packet & !test_pass) return(NULL)

    stopifnot(test_pass)

    payload <- readBin(
      payload, "integer", length(payload) / 2, 2, TRUE
    )

    payload <- matrix(
      round(payload / scale_factor, 3),
      ncol = 3,
      byrow = TRUE
    )
  } else {

    payload <- matrix(NA, info$Sample_Rate, 3)

  }

  return(
    stats::setNames(
      data.frame(payload, row.names = NULL),
      c("Accelerometer_X", "Accelerometer_Y", "Accelerometer_Z")
    )
  )

}
