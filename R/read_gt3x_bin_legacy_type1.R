#' @rdname parse_log_bin
#' @keywords internal
legacy_bin_type1 <- function(log, tz, verbose = FALSE, include, info) {

  ## Get headers

    record_headers <- get_headers(log, tz, verbose)
    record_headers <- sort_records(record_headers)
    record_headers <- select_records(record_headers, include)

  ## Get parameters (if applicable)

    if ("PARAMETERS" %in% names(record_headers)) {
      parameters <- parse_packet_set(
        record_headers$PARAMETERS, log,
        tz, verbose
      )
      record_headers$PARAMETERS <- NULL
    } else {
      parameters <- NULL
    }

  ## Get schema (if applicable)

    if ("SENSOR_SCHEMA" %in% names(record_headers)) {
      schema <- parse_packet_set(
        record_headers$SENSOR_SCHEMA, log,
        tz, verbose
      )
      record_headers$SENSOR_SCHEMA <- NULL
    } else {
      schema <- NULL
    }

  ## Get events (if applicable)

    if (!"EVENT" %in% names(record_headers)) {
      events <- parse_packet_set(
        structure(list(), class = "EVENT"),
        log, tz, verbose
      )
    } else {
      events <- parse_packet_set(
        record_headers$EVENT,
        log, tz, verbose, info = info
      )
    }
    record_headers$EVENT <- NULL

  ## Now process the remaining packets

    results <- lapply(
      record_headers,
      parse_packet_set,
      log = log, tz = tz,
      verbose = verbose, info = info,
      parameters = parameters, schema = schema,
      events = events, raw_method = 1
    )

    if(all("PARAMETERS" %in% include, exists("parameters"))) {
      results$PARAMETERS <- parameters
    }
    if(all("SENSOR_SCHEMA" %in% include, exists("schema"))) {
      results$SENSOR_SCHEMA <- schema
    }
    if(all("EVENT" %in% include, exists("events"))) {
      results$EVENT <- events
    }

    new_names <- sapply(results, function(x) class(x)[1])
    new_names <- unname(
      ifelse(new_names == "NULL", names(new_names), new_names)
    )

    if (verbose) cat("\n")
    results %<>% stats::setNames(new_names)
    results$info <- info
    results

}
