external_parser <- function(file, tz, verbose, flag_idle_sleep = FALSE, ...) {

  read.gt3x::read.gt3x(
    file, verbose, asDataFrame = TRUE,
    imputeZeroes = TRUE, ...
  ) %>%
  latch_gt3x(verbose = verbose, flag_idle_sleep = flag_idle_sleep) %>%
  dplyr::rename(
    "Timestamp" = "time", "Accelerometer_X" = "X",
    "Accelerometer_Y" = "Y", "Accelerometer_Z" = "Z"
  ) %T>%
  {stopifnot(exists("Timestamp", .))} %>%
  within({Timestamp = lubridate::force_tz(Timestamp, tz)}) %>%
  external_restructure(.)

}

external_restructure <- function(AG) {

  info <-
    attributes(AG) %>%
    {.[setdiff(names(.), c("names", "row.names", "class"))]} %>%
    c(stringsAsFactors = FALSE) %>%
    do.call(data.frame, .) %>%
    stats::setNames(., gsub("^header\\.", "", names(.))) %>%
    stats::setNames(., gsub("\\.+", "_", names(.))) %>%
    .[ ,!duplicated(tolower(names(.)))] %>%
    structure(., row.names = 1:nrow(.))

  attributes(AG) %<>% .[c("names", "row.names", "class")]

  structure(AG, class = c("RAW", "data.frame")) %>%
  list(info = info, RAW = .)

}
