external_parser <- function(log, info, file, tz, verbose, ...) {


  events <-
    list(EVENT = type3(log, verbose)) %!>%
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
    c(attr(AG, "stop_time"), attr(AG, "last_sample_time")) %>%
    .[.>0] %>%
    min(.) %>%
    lubridate::force_tz(tz) %>%
    get_expected(
      start = lubridate::force_tz(
        attr(AG, "start_time"),
        tz
      ),
      end = .,
      samp_rate = attr(AG, "sample_rate")
    ) %>%
    {data.frame(Timestamp = .$expected_full)} %>%
    dplyr::full_join(AG, ., "Timestamp") %>%
    dplyr::slice(., order(.$Timestamp)) %>%
    within({Timestamp = lubridate::force_tz(Timestamp, tz)})


  if (nrow(events$other_events) > 0) {
    zeroes <-
      {events$other_events$payload_size == 1} %>%
      events$other_events$timestamp[.] %>%
      {AG$Timestamp %in% .}
    AG[zeroes, .accel_names] <- 0
  }


  impute_primary(AG, verbose) %!>%
  external_restructure(.) %!>%
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
