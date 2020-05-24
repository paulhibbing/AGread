rm(list = ls())
library(magrittr)
devtools::load_all()
# load("data-raw/example_data.RData")

# Automated ---------------------------------------------------------------

# legacy <- read_gt3x(
#   file = "data-raw/119AGBPFLW (2016-03-08).gt3x",
#   tz = "UTC",
#   verbose = TRUE, parser = "legacy"
# )
#
# dev <- read_gt3x(
#   file = "data-raw/119AGBPFLW (2016-03-08).gt3x",
#   tz = "UTC",
#   verbose = TRUE, parser = "dev"
# )

# Interactive -------------------------------------------------------------

## inputs

  file <- "data-raw/internal_tests/test7.gt3x"
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
  parser <- "dev"

## read_gt3x

  file %<>% read_gt3x_setup(verbose)

  info <- read_gt3x_info(file, tz, verbose)

  log_file  <-
    file$path %>%
    utils::unzip("log.bin", exdir = tempdir())

## parse_log_bin

  include %<>% validate_include(verbose)
  parser  %<>% validate_parser(.)

  if (verbose) cat("\n  Reading log.bin")
  log <- readBin(
    log_file, "raw", file$result["log.bin", "Length"]
  )
  if (verbose) cat("  ............. COMPLETE")

dev_bin_type1
# record_set <- record_headers[[6]]
# record_header <- record_set[20, ]
# do_post_process <- TRUE

# payload <- payload_raw
