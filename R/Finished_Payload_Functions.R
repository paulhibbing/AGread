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
