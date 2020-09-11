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
      list(RAW = .) %>% ## New name
      c(packets, .) %>%
      .[names(.) != "ACTIVITY2"] %>% ## Remove binary data
      {.[!sapply(., is.null)]}

  ## Get ACTIVITY (if applicable)

    packets %<>%
      get_activity(tz, info, verbose) %>%
      list(parsed_activity = .) %>% ## Temp name
      c(packets, .) %>%
      .[names(.) != "ACTIVITY"] %>% ## Remove binary data
      {.[!sapply(., is.null)]}

    if ("parsed_activity" %in% names(packets) & "RAW" %in% names(packets)) {

      warning(
        "File contains both ACTIVITY and",
        " ACTIVITY2 packets:\n  Returning ",
        "ACTIVITY2 as `packets$RAW` and\n  ",
        "ACTIVITY as `packets$ACTIVITY`"
      )

      names(packets) %<>% gsub(
        "^parsed_activity$", "ACTIVITY", .
      )

    } else {

      names(packets) %<>% gsub(
        "^parsed_activity$", "RAW", .
      )

    }

  ## Get SENSOR_DATA (if applicable)

    packets %<>%
      get_sensor_data(
        schema, parameters, tz, info, verbose
      ) %>%
      .[ ,names(.) != "Discard"] %>%
      list(IMU = .) %>%
      c(packets, .) %>%
      .[names(.) != "SENSOR_DATA"] %>%
      {.[!sapply(., is.null)]}

  ## Get remaining packets (if applicable)

    remaining <-
      c("PARAMETERS", "SENSOR_SCHEMA", "EVENT") %>%
      setdiff(.packets, .) %>%
      intersect(names(packets))

    existing <-
      names(packets) %>%
      setdiff(remaining)

    sapply(
      remaining,
      dev_bin1_map_packets,
      packets = packets,
      tz = tz,
      info = info,
      verbose = verbose,
      simplify = FALSE
    ) %>%
    c(packets[existing], ., info = list(info)) %T>%
    {if (verbose) cat("\n")}

}
