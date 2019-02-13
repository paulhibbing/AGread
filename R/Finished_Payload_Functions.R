payload_parse_battery_2 <- function(payload, info) {
  paste(
    "BATTERY VOLTAGE:",
    readBin(payload, "integer", 2, 2, FALSE) / 1000
  )
}

payload_parse_event_3 <- function(payload, info) {
  return(list(
    payload = paste(as.character(payload), collapse = " "),
    description = "ActiGraph internal use. No further documentation."
  ))
}

payload_parse_infodata_6 <- function(payload, info) {
  rawToChar(payload)
}

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

payload_parse_activity2_26 <- function(payload, info, is_last_packet = FALSE) {
  
  test_pass <- all(
    length(payload) %% 3 == 0,
    length(payload) %% 2 == 0
  )
  if (is_last_packet & !test_pass) return(NULL)
  
  stopifnot(test_pass)
  
  scale_factor <- switch(
    substring(info$Serial_Number, 1, 3),
    "NEO" = 341,
    "CLE" = 341,
    "MOS" = 256
  )
  
  if ("Acceleration_Scale" %in% names(info)) {
    scale_factor <- info$Acceleration_Scale
  }
  
  payload <- readBin(
    payload, "integer", length(payload) / 2, 2, TRUE
  )
  
  payload <- matrix(
    round(payload / scale_factor, 3),
    ncol = 3,
    byrow = TRUE
  )
  
  return(
    stats::setNames(
      data.frame(payload, row.names = NULL),
      c("Accelerometer_X", "Accelerometer_Y", "Accelerometer_Z")
    )
  )
  
}