payload_parse_activity_0 <- function(payload, info) {
  message(paste(
    "PARSER NOT DEVELOPED YET:",
    "One second of raw activity samples packed into",
    "12-bit values in YXZ order."
  ))
}

payload_parse_heart_rate_4 <- function(payload, info) {
  message(paste(
    "PARSER NOT DEVELOPED YET:",
    "0x04	HEART_RATE_BPM	Heart rate average beats per",
    "minute (BPM) as one byte unsigned integer."
  ))
}

payload_parse_lux_5 <- function(payload, info) {
  message(paste(
    "PARSER NOT DEVELOPED YET:",
    "0x05	LUX	Lux value as a little-endian",
    "unsigned short (2 bytes)."
  ))
}

payload_parse_tag_7 <- function (payload, info) {
  message(paste(
    "PARSER NOT DEVELOPED YET:",
    "0x07	TAG	13 Byte Serial, 1 Byte Tx Power, 1 Byte",
    "(signed) RSSI"
  ))
    
}

payload_parse_epoch_9 <- function(payload, info) {
  message(paste(
    "PARSER NOT DEVELOPED YET:",
    "PARSER NOT DEVELOPED YET:",
    "0x09	EPOCH	60-second epoch data"
  ))
}

payload_parse_heart_ant_11 <- function(payload, info) {
  message(paste(
    "PARSER NOT DEVELOPED YET:",
    "0x0B	HEART_RATE_ANT	Heart Rate RR information",
    "from ANT+ sensor."
  ))
}

payload_parse_epoch_12 <- function(payload, info) {
  message(paste(
    "PARSER NOT DEVELOPED YET:",
    "0x0C	EPOCH2	60-second epoch data" 
  ))
}

payload_parse_heart_ble_14 <- function(payload, info) {
  message(paste(
    "PARSER NOT DEVELOPED YET:",
    "0x0E	HEART_RATE_BLE Bluetooth heart rate information",
    "(BPM and RR). This is a Bluetooth standard format."
  ))
}

payload_parse_epoch_15 <- function(payload, info) {
  message(paste(
    "PARSER NOT DEVELOPED YET:",
    "0x0F	EPOCH3	60-second epoch data" 
  ))
}

payload_parse_epoch_16 <- function(payload, info) {
  message(paste(
    "PARSER NOT DEVELOPED YET:",
    "0x10	EPOCH4	60-second epoch data" 
  ))
}
