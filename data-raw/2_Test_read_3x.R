rm(list = ls())
library(magrittr)
devtools::load_all()
# load("data-raw/example_data.RData")

# Automated ---------------------------------------------------------------

# dev <- read_gt3x(
#   file = "C:/Users/prhibbing/Downloads/SFD-26-CB (2018-06-21).gt3x",
#   tz = "UTC",
#   verbose = TRUE, parser = "dev"
# )
#
# legacy <- read_gt3x(
#   file = "C:/Users/prhibbing/Downloads/SFD-26-CB (2018-06-21).gt3x",
#   tz = "UTC",
#   verbose = TRUE, parser = "legacy"
# )

# Interactive -------------------------------------------------------------

AG <- read_AG_raw(
  "data-raw/internal_tests/TAS1F07170345 (2018-12-06)RAW.csv",
  verbose = TRUE, return_raw = TRUE
)

## inputs

  file <- "data-raw/internal_tests/TAS1F07170345 (2018-12-06).gt3x"
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
  cleanup <- FALSE

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

## Error is 9847262

  log[9847263] ## Separator
  log[9847264] ## Type (ACTIVITY2)

  log[9847265:9847268] %>% ## Timestamp ("2018-11-30 17:31:05 UTC")
    rev(.) %>%
    as.character(.) %>%
    paste(collapse = "") %>%
    paste0("0x", .) %>%
    strtoi(.) %>%
    anytime::anytime("UTC")

  log[9847269:9847270] %>% ## Length (180)
    rev(.) %>%
    as.character(.) %>%
    paste(collapse = "") %>%
    paste0("0x", .) %>%
    strtoi(.)

  log[9847270+1:180] ## Payload

  log[9847451] ## Checksum

  checksumC(log, 9847262, 9847450) ## c++ indices zero-indexed

## Next record (sanity check)

  log[9847452] ## Next record
  log[9847453] ## Type (ACTIVITY2)

  log[9847454:9847457] %>% ## Timestamp ("2018-11-30 17:31:06 UTC")
    rev(.) %>%
    as.character(.) %>%
    paste(collapse = "") %>%
    paste0("0x", .) %>%
    strtoi(.) %>%
    anytime::anytime("UTC")

  log[9847458:9847459] %>% ## Length (180)
    rev(.) %>%
    as.character(.) %>%
    paste(collapse = "") %>%
    paste0("0x", .) %>%
    strtoi(.)

  log[9847459+1:180] ## Payload

  log[9847640] ## Checksum

  checksumC(log, 9847451, 9847639) ## c++ indices zero-indexed
