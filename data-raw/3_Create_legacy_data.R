# packageVersion("AGread") ## Should be 0.1.2
do_start <- svDialogs::dlgMessage(
  paste(
    "RUN THIS FILE OUTSIDE (!!!) THE AGREAD PROJECT",
    "FILE, USING AGREAD VERSION 0.1.2. Press cancel",
    "to abort and start over.",
    sep = "\n"
  ),
  "okcancel"
)$res == "ok"

if (do_start) {
  rm(list = ls())
  setwd(
    "C:/Users/aplstudy/Desktop/AGread"
  )

  counts <- AGread::read_AG_counts(
    "inst/extdata/example1sec.csv", verbose = TRUE, skip = 11
  )
  saveRDS(counts, file = "data-raw/counts_legacy.rds")

  raw <- AGread::read_AG_raw(
    "inst/extdata/TestID_LeftWrist_RAW.csv", verbose = TRUE
  )[ ,-2]
  raw$Timestamp <- lubridate::force_tz(raw$Timestamp, "UTC")
  saveRDS(raw, file = "data-raw/RAW_legacy.rds")

  imu <- AGread::read_AG_IMU(
    "inst/extdata/example-IMU.csv", verbose = TRUE
  )[ ,-2]
  imu$Timestamp <- lubridate::force_tz(imu$Timestamp, "UTC")
  saveRDS(imu, file = "data-raw/IMU_legacy.rds")
}
