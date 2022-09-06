external_parser <- function(log, file, tz, verbose, ...) {


  events <-
    list(EVENT = type3(log, verbose)) %>%
    get_events(tz, info, verbose)


  AG <-
    read.gt3x::read.gt3x(
      file$path, verbose = FALSE, asDataFrame = TRUE,
      imputeZeroes = FALSE, ...
    ) %>%
    dplyr::rename(
      "Timestamp" = "time", "Accelerometer_X" = "X",
      "Accelerometer_Y" = "Y", "Accelerometer_Z" = "Z"
    ) %T>%
    {stopifnot(exists("Timestamp", .))}


  AG <-
    get_expected(
      start = attr(AG, "start_time"),
      end = min(attr(AG, "stop_time"), attr(AG, "last_sample_time")),
      samp_rate = attr(AG, "sample_rate")
    ) %>%
    {data.frame(Timestamp = .$expected_full)} %>%
    dplyr::full_join(AG, ., "Timestamp") %>%
    {.[order(.$Timestamp), ]} %>%
    within({Timestamp = lubridate::force_tz(Timestamp, tz)})


  if (nrow(events$other_events) > 0) {
    zeroes <-
      lubridate::floor_date(AG$Timestamp, "1 sec") %in%
      events$other_events$timestamp
    AG[zeroes, .accel_names] <- 0
  }


  if (!isTRUE(requireNamespace("zoo", quietly = TRUE))) stop(
    "Please install the `zoo` package to",
    " enable latching of missing gt3x values."
  )


  AG$Accelerometer_X <- zoo::na.locf0(AG$Accelerometer_X)
  AG$Accelerometer_Y <- zoo::na.locf0(AG$Accelerometer_Y)
  AG$Accelerometer_Z <- zoo::na.locf0(AG$Accelerometer_Z)


  external_restructure(AG) %>%
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
