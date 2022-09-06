external_parser <- function(
  log, file, tz, verbose,
  flag_idle_sleep = FALSE, ...
) {


  events <-
    list(EVENT = type3(log, verbose)) %>%
    get_events(tz, info, verbose)


  read.gt3x::read.gt3x(
    file$path, verbose = FALSE, asDataFrame = TRUE,
    imputeZeroes = TRUE, ...
  ) %>%
  dplyr::rename(
    "Timestamp" = "time", "Accelerometer_X" = "X",
    "Accelerometer_Y" = "Y", "Accelerometer_Z" = "Z"
  ) %T>%
  {stopifnot(exists("Timestamp", .))} %>%
  flag_idle(events) %>%
  {latch_gt3x(
    ., flag_idle_sleep = flag_idle_sleep,
    is_sleep = .$idle
  )} %>%
  within({Timestamp = lubridate::force_tz(Timestamp, tz)}) %>%
  external_restructure(.) %>%
  c(list(EVENT = events))


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
