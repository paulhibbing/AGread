# Single value parsers ----------------------------------------------------

#' Parse PARAMETERS packet for battery state
#'
#' @param value portion of payload to parse
#' @param key PARAMETERS key entry to use for guidance
#'
#' @keywords internal
#'
par_battery_state <- function(value, key) {
  # value <- test_val("02 00 00 00")
  # key <- subset(PARAMETERS, Label == toupper("battery_state"))
  value <- get_value(key$Type[1], value)
  key <- key[value, ]
  value <- gsub("[0-9]\\. ", "", key$Range)
  return(update_key(key, value))
}

#' Parse PARAMETERS packet for battery voltage
#'
#' @inheritParams par_battery_state
#'
#' @keywords internal
#'
par_battery_voltage <- function(value, key) {
  # value <- test_val("81 95 41 03")
  # key <- subset(PARAMETERS, Label == "BATTERY_VOLTAGE")
  value <- get_value(key$Type[1], value)
  return(update_key(key, value))
}

#' Parse PARAMETERS packet for calibration time
#'
#' @inheritParams par_battery_state
#' @param tz character. Time zone to use.
#'
#' @keywords internal
#'
par_calibration_time <- function(value, key, tz) {
  # value <- test_val("27 AA 0D 54")
  # key <- subset(PARAMETERS, Label == "CALIBRATION_TIME")
  value <- as.character(anytime::anytime(
    readBin(value, "integer", 4, 4),
    tz
  ))
  return(update_key(key, value))
}

#' Parse PARAMETERS packet for firmware version
#'
#' @inheritParams par_battery_state
#'
#' @keywords internal
#'
par_firmware_version <- function(value, key) {
  # value <- test_val("25 00 01 01")
  # key <- subset(PARAMETERS, Label == "FIRMWARE_VERSION")
  major <- readBin(value[4], "integer", 1, 1, FALSE)
  minor <- readBin(value[3], "integer", 1, 1, FALSE)
  build <- readBin(value[1], "integer", 1, 1, FALSE)
  value <- paste(
    c(
      stringr::str_pad(major, 2, pad = "0"),
      stringr::str_pad(minor, 2, pad = "0"),
      stringr::str_pad(build, 2, pad = "0")
    ),
    collapse = "."
  )
  return(update_key(key, value))
}

#' Parse PARAMETERS packet for memory size
#'
#' @inheritParams par_battery_state
#'
#' @keywords internal
#'
par_memory_size <- function(value, key) {
  # value <- test_val("00 00 80 E4")
  # key <- subset(PARAMETERS, Label == "MEMORY_SIZE")
  value_bin <- AG_binary(value)
  value <- bin_int(value_bin)
  value <- paste(round(value / (1024^3), 2), "GB")
  return(update_key(key, value))
}

#' Parse PARAMETERS packet for wireless serial number
#'
#' @inheritParams par_battery_state
#'
#' @keywords internal
#'
par_wireless_serial_number <- function(value, key) {
  # value <- test_val("44 D4 12 AF")
  # key <- subset(PARAMETERS, Label == "WIRELESS_SERIAL_NUMBER")
  value_bin <- AG_binary(value)
  value <- bin_int(value_bin)
  return(update_key(key, value))
}

# Switch-based parsers ----------------------------------------------------

#' Parse PARAMETERS packet for feature capabilities
#'
#' @inheritParams par_battery_state
#'
#' @keywords internal
#'
par_feature_capabilities <- function(value, key) {
  # value <- test_val("7D 01 00 00")
  # key <- subset(PARAMETERS, Label == "FEATURE_CAPABILITIES")
  value_bin <- rev(AG_binary(value))[1:9]
  caps <- which(value_bin)

  if (!length(caps)) return(update_key(key, "none"))

  value <- sapply(
    caps,
    function(x) switch(
      x, "heart rate monitor", "data summary", "sleep mode",
      "proximity tagging", "epoch data", "no raw data", "IMU",
      "spare", "configurable proximity interval"
    )
  )

  value <- capability_collapse(value)
  return(update_key(key, value))
}

#' Parse PARAMETERS packet for display capabilities
#'
#' @inheritParams par_battery_state
#'
#' @keywords internal
#'
par_display_capabilities <- function(value, key) {
  # value <- test_val("07 00 00 00")
  # key <- subset(PARAMETERS, Label == "DISPLAY_CAPABILITIES")

  value_bin <- rev(AG_binary(value))[1:4]
  caps <- which(value_bin)
  if (!length(caps)) return(update_key(key, "none"))

  value <- sapply(
    caps,
    function(x) switch(
      x, "display on/off", "12/24-hour time",
      "feedback on/off", "kcals on/off"
    )
  )

  value <- capability_collapse(value)
  return(update_key(key, value))
}

#' Parse PARAMETERS packet for wireless mode
#'
#' @inheritParams par_battery_state
#'
#' @keywords internal
#'
par_wireless_mode <- function(value, key) {
  # value <- test_val("00 00 00 00")
  # key <- subset(PARAMETERS, Label == "WIRELESS_MODE")
  value <- get_value(key$Type[1], value)
  value <- switch(value + 1, "Disabled", "Central", "Peripheral")
  return(update_key(key, value))
}

#' Parse PARAMETERS packet for enabled features
#'
#' @inheritParams par_battery_state
#'
#' @keywords internal
#'
par_feature_enable <- function(value, key) {
  # value <- test_val("14 01 00 00")
  # key <- subset(PARAMETERS, Label == "FEATURE_ENABLE")
  value_bin <- rev(AG_binary(value))[1:6]

  features <- which(value_bin)
  if (!length(features)) return(update_key(key, "none"))

  value <- sapply(
    features,
    function(x) switch(
        x, "heart rate monitor", "data summary", "sleep mode",
        "proximity tagging", "epoch data", "no raw data"
    )
  )
  value <- capability_collapse(value)
  return(update_key(key, value))
}

#' Parse PARAMETERS packet for display configuration
#'
#' @inheritParams par_battery_state
#'
#' @keywords internal
#'
par_display_configuration <- function(value, key) {
  # value <- test_val("07 00 00 00")
  # key <- subset(PARAMETERS, Label == "DISPLAY_CONFIGURATION")
  value_bin <- rev(AG_binary(value))[1:4]
  config <- which(value_bin)

  if (!length(config)) return(update_key(key, "12-hour time"))
  if (!2 %in% config) config <- c(config, 5)

  value <- sapply(
    config,
    function(x) switch(
      x, "display on", "24-hour time", "feedback on",
      "kcals on", "12-hour time"
    )
  )
  value <- capability_collapse(value)
  return(update_key(key, value))
}

#' Parse PARAMETERS packet for sensor configuration
#'
#' @inheritParams par_battery_state
#'
#' @keywords internal
#'
par_sensor_configuration <- function(value, key) {
  # value <- test_val("00 00 00 00")
  # key <- subset(PARAMETERS, Label == toupper("sensor_configuration"))

  value_bin <- rev(AG_binary(value))[1:4]
  config <- which(value_bin)
  if (!length(config)) return(update_key(key, "IMU off"))

  value <- sapply(
    config,
    function(x) switch(
        x, "IMU accelerometer", "IMU gyroscope",
        "IMU magnetometer", "IMU temperature"
    )
  )
  value <- capability_collapse(value)
  return(update_key(key, value))
}
