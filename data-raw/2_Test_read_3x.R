# Interactive -------------------------------------------------------------
rm(list = ls())
devtools::load_all()

# file <- "119AGBPFLW (2016-03-08).gt3x"
# verbose <- TRUE
# tz <- "UTC"
# give_timestamp <- TRUE
# include <- c(
#   "METADATA", "PARAMETERS", "SENSOR_SCHEMA", "BATTERY", "EVENT",
#   "TAG", "ACTIVITY", "HEART_RATE_BPM", "HEART_RATE_ANT", "HEART_RATE_BLE",
#   "LUX", "CAPSENSE", "EPOCH", "EPOCH2", "EPOCH3", "EPOCH4", "ACTIVITY2",
#   "SENSOR_DATA"
# )
# n_records <- file_3x["log.bin", "Length"]

test <- read_gt3x(
  file = "data-raw/119AGBPFLW (2016-03-08).gt3x",
  tz = "UTC",
  verbose = TRUE, give_timestamp = TRUE)#,
#   include = c("METADATA", "PARAMETERS", "SENSOR_SCHEMA", "BATTERY",
#   "TAG", "ACTIVITY", "HEART_RATE_BPM", "HEART_RATE_ANT", "HEART_RATE_BLE",
#   "LUX", "CAPSENSE", "EPOCH", "EPOCH2", "EPOCH3", "EPOCH4", "ACTIVITY2"
#   )
# )