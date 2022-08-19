# Helper for switch-based parsers -----------------------------------------

process_par_configs <- function(
    x, key, ..., none = "none", check_2 = FALSE
) {

  if (!length(x)) return(update_key(key, none))
  if (check_2 & !2 %in% x) x %<>% c(2)

  x %>%
  sapply(switch, ...) %>%
  capability_collapse(.) %>%
  update_key(key, .)

}


# Single value parsers ----------------------------------------------------

par_battery_state <- function(value, key) {

  value <- get_value(key$Type[1], value)
  if (value == 0) {
    key <- key[1, ]
    value <- paste(
      "ERROR: value of", value, "recorded in the packet"
    )
  } else {
    key <- key[value, ]
    value <- gsub("[0-9]\\. ", "", key$Range)
  }

  update_key(key, value)

}


par_battery_voltage <- function(value, key) {

  get_value(key$Type[1], value) %>%
  update_key(key, .)

}


par_calibration_time <- function(value, key, tz) {

  readBin(value, "integer", 4, 4) %>%
  anytime::anytime(tz) %>%
  as.character(.) %>%
  update_key(key, .)

}


par_firmware_version <- function(value, key) {

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

  update_key(key, value)

}


par_memory_size <- function(value, key) {

  logical_bits(value) %>%
  bin_int(.) %>%
  {. / (1024^3)} %>%
  round(2) %>%
  paste("GB") %>%
  update_key(key, .)

}


par_wireless_serial_number <- function(value, key) {

  logical_bits(value) %>%
  bin_int(.) %>%
  update_key(key, .)

}

# Switch-based parsers ----------------------------------------------------

par_feature_capabilities <- function(value, key) {

  which_bits(value, 9) %>%
  process_par_configs(
    key, "heart rate monitor", "data summary", "sleep mode",
    "proximity tagging", "epoch data", "no raw data", "IMU",
    "spare", "configurable proximity interval"
  )

}


par_display_capabilities <- function(value, key) {

  which_bits(value, 4) %>%
  process_par_configs(
    key, "display on/off", "12/24-hour time",
    "feedback on/off", "kcals on/off"
  )

}


par_feature_enable <- function(value, key) {

  which_bits(value, 6) %>%
  process_par_configs(
    key, "heart rate monitor", "data summary", "sleep mode",
    "proximity tagging", "epoch data", "no raw data"
  )

}


par_display_configuration <- function(value, key) {

  which_bits(value, 4) %>%
  process_par_configs(
    key, "display on", "24-hour time",
    "feedback on", "kcals on", "12-hour time",
    none = "12-hour time", check_2 = TRUE
  )

}


par_sensor_configuration <- function(value, key) {

  which_bits(value, 4) %>%
  process_par_configs(
    key, "IMU accelerometer", "IMU gyroscope",
    "IMU magnetometer", "IMU temperature",
    none = "IMU off"
  )

}


par_wireless_mode <- function(value, key) {

  get_value(key$Type[1], value) %>%
  {. + 1} %>%
  switch("Disabled", "Central", "Peripheral") %>%
  update_key(key, .)

}
