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
    if (all(
      "PARAMETERS" %in% include,
      !is.null(parameters)
    )) {
      packets$PARAMETERS <- parameters
    }

  ## Get schema (if applicable)

    schema <- get_schema(packets, tz, verbose)
    if (all(
      "SENSOR_SCHEMA" %in% include,
      !is.null(schema)
    )) {
      packets$SENSOR_SCHEMA <- schema
    }

  ## Get events (if applicable)

    events <- get_events(packets, tz, info, verbose)
    if (all(
      "EVENT" %in% include,
      !is.null(events)
    )) {
      packets$EVENT <- events
    }

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

  ## Get remaining packets (if applicable)

    packets <-
      c(
        "PARAMETERS", "SENSOR_SCHEMA", "EVENT",
        "RAW", "IMU"
      ) %>%
      setdiff(.packets, .) %>%
      intersect(names(packets)) %>%
      sapply(
        dev_bin1_map_packets,
        packets = packets,
        tz = tz,
        verbose = verbose,
        simplify = FALSE
      ) %>%
      c(packets, .)

  ## Clean up

    new_names <- sapply(packets, function(x) class(x)[1])
    new_names <- unname(
      ifelse(new_names == "NULL", names(new_names), new_names)
    )

    if (verbose) cat("\n")
    packets %<>% stats::setNames(new_names)
    packets$info <- info
    packets

}
