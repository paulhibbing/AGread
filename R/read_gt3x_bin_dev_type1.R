#' @rdname parse_log_bin
#' @keywords internal
dev_bin_type1 <- function(log, tz, verbose, include, info) {

  ## Parse log.bin, then format for further processing

    packets <-
      cpp_include(include) %>%
      bin_dev1_initialize(log, verbose, .)

    packets %<>%
      sapply(function(x) x$type) %>%
      split(packets, .) %>%
      stats::setNames(
        ., .packets[match(names(.), .numbers)]
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

}
