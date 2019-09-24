rm(list = ls())
devtools::load_all()

# Off-the-bat -------------------------------------------------------------

  # file <- "inst/extdata/example.gt3x"
  file <- "data-raw/119AGBPFLW (2016-03-08).gt3x"
  # file <- "data-raw/broken-8/TAS1H30182785 (2019-09-17).gt3x"
  tz <- "UTC"
  verbose <- FALSE
  include <- c(
    "METADATA", "PARAMETERS", "SENSOR_SCHEMA", "BATTERY",
    "EVENT", "TAG", "ACTIVITY", "HEART_RATE_BPM",
    "HEART_RATE_ANT", "HEART_RATE_BLE", "LUX", "CAPSENSE",
    "EPOCH", "EPOCH2", "EPOCH3", "EPOCH4", "ACTIVITY2",
    "SENSOR_DATA"
  )

# Pre-parse_log_bin -------------------------------------------------------

  file_3x_len <- file_3x["log.bin", "Length"]

# Comparison --------------------------------------------------------------

  all_3x <- read_gt3x(
    # "data-raw/broken-8/TAS1H30182785 (2019-09-17).gt3x",
    # "data-raw/119AGBPFLW (2016-03-08).gt3x",
    "inst/extdata/example.gt3x",
    include = setdiff(include, "SENSOR_DATA"),
    verbose = TRUE
  )
  raw_3x <- all_3x$RAW
  class(raw_3x) <- "data.frame"

  raw_csv <- read_AG_raw(
    # "data-raw/broken-8/TAS1H30182785 (2019-09-17)RAW.csv",
    # "data-raw/119AGBPFLW (2016-03-08)RAW.csv",
    "inst/extdata/exampleRAW.csv",
    verbose = TRUE, return_raw = TRUE
  )

  all.equal(
    raw_3x[seq(nrow(raw_csv)), ], raw_csv[ ,names(raw_3x)],
    scale = 1, tolerance = 0.0015
  )

  PAutilities::test_errors(
    raw_3x[seq(nrow(raw_csv)), ], raw_csv,
    .accel_names, return_logical = FALSE
  )
