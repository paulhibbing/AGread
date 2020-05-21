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

  ## Define expected timestamps

    all_times <- seq(
      info$Start_Date,
      info$Last_Sample_Time,
      "1 sec"
    )

  ## Get parameters (if applicable)

    parameters <- get_parameters(packets, verbose)
    packets$PARAMETERS <- NULL

  ## Get schema (if applicable)

    schema <- get_schema(packets, verbose)
    packets$SENSOR_SCHEMA <- NULL

  ## Get events (if applicable)

    events <- get_events(packets, tz, info, verbose)
    packets$EVENT <- NULL

  ## Get ACTIVITY2 (if applicable)

    raw <- get_activity2(
      packets, all_times, tz, info, verbose
    )

}
