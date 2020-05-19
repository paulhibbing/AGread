rm(list = ls())
library(magrittr)
devtools::load_all()
# load("data-raw/example_data.RData")

# Automated ---------------------------------------------------------------

# test <- read_gt3x(
#   file = "data-raw/119AGBPFLW (2016-03-08).gt3x",
#   tz = "UTC",
#   verbose = TRUE, give_timestamp = TRUE#,
#   # include = c(
#   #     "PARAMETERS", "SENSOR_SCHEMA", "BATTERY", "EVENT",
#   #     "LUX", "CAPSENSE", "ACTIVITY2"
#   #   )
# )

# Interactive -------------------------------------------------------------

file <- "data-raw/119AGBPFLW (2016-03-08).gt3x"
verbose <- TRUE
tz <- "UTC"
give_timestamp <- TRUE
include <- c(
  "METADATA", "PARAMETERS", "SENSOR_SCHEMA", "BATTERY", "EVENT",
  "TAG", "ACTIVITY", "HEART_RATE_BPM", "HEART_RATE_ANT", "HEART_RATE_BLE",
  "LUX", "CAPSENSE", "EPOCH", "EPOCH2", "EPOCH3", "EPOCH4", "ACTIVITY2",
  "SENSOR_DATA"
)
flag_idle_sleep <- FALSE
parser <- "legacy"

# record_set <- record_headers[[6]]
# record_header <- record_set[20, ]
# do_post_process <- TRUE

# payload <- payload_raw
