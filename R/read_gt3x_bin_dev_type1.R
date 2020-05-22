#' @rdname parse_log_bin
#' @keywords internal
dev_bin_type1 <- function(log, tz, verbose, include, info) {

  ## Parse log.bin, then format for further processing

    packets <-
      cpp_include(include) %>%
      dev1_bin_initialize(log, verbose, .)

    packets %<>%
      sapply(function(x) x$type) %>%
      split(packets, .) %>%
      stats::setNames(
        ., .packets[match(names(.), .numbers)]
      )

  ## Get parameters (if applicable)

    parameters <- get_parameters(packets, tz, verbose)
    packets$PARAMETERS <- NULL

  ## Get schema (if applicable)

    schema <- get_schema(packets, tz, verbose)
    packets$SENSOR_SCHEMA <- NULL

  ## Get events (if applicable)

    events <- get_events(packets, tz, info, verbose)
    packets$EVENT <- NULL

  ## Get ACTIVITY2 (if applicable)

    packets %<>%
      get_activity2(tz, info, verbose) %>%
      list(RAW = .) %>%
      c(packets, .) %>%
      .[names(.) != "ACTIVITY2"]

  ## Get SENSOR_DATA (if applicable)

    packets %<>%
      get_sensor_data(schema, tz, info, verbose) %>%
      .[ ,names(.) != "Discard"] %>%
      list(IMU = .) %>%
      c(packets, .) %>%
      .[names(.) != "SENSOR_DATA"]

}
