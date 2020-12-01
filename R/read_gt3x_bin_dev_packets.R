#' @rdname dev_bin_packets
#' @param label character. The desired class name for parsed packet data of a
#'   given type
#' @keywords internal
set_packet_class <- function(packets, label) {
  packets %>%
  structure(
    .,
    class = append(class(.), label, 0)
  )
}

#' Parse gt3x packets using the dev scheme
#' @rdname dev_bin_packets
#' @param packets object containing data packets or subsequently parsed data
#' @inheritParams read_gt3x
#' @keywords internal
get_parameters <- function(packets, tz, verbose) {

  if (!"PARAMETERS" %in% names(packets)) {
    if (verbose) cat(
      "\n  No PARAMETERS packet to parse (or it\'s",
      "been left out of `include`)"
    )
    return(NULL)
  }

  if (verbose) packet_print("startup", "PARAMETERS")

  packets$PARAMETERS %T>%
  {stopifnot(length(.) == 1)} %>%
  {.[[1]]$payload} %>%
  split(., cumsum(seq(.) %% 8 == 1)) %>%
  {do.call(rbind, lapply(., process_parameters, tz = tz))} %>%
  {.[.$Label != "UNUSED KEY", ]} %>%
  {stats::setNames(as.list(.$value), .$Label)} %>%
  set_packet_class("PARAMETERS") %T>%
  {if (verbose) packet_print("cleanup", "PARAMETERS")}

}

#' @rdname dev_bin_packets
#' @keywords internal
get_schema <- function(packets, tz, verbose) {

  if (!"SENSOR_SCHEMA" %in% names(packets)) {
    if (verbose) cat(
      "\n  No SENSOR_SCHEMA packet to parse (or it\'s",
      "been left out of `include`)"
    )
    return(NULL)
  }

  packets$SENSOR_SCHEMA %T>%
  {stopifnot(length(.) == 1)} %>%
  {.[[1]]$payload} %>%
  parse_packet_set(
    structure(list(), class = "SENSOR_SCHEMA"),
    NULL, tz, verbose, .
  ) %T>%
  {if (verbose) packet_print("cleanup", "SENSOR_SCHEMA")}

}

#' @rdname dev_bin_packets
#' @keywords internal
get_events <- function(packets, tz, info, verbose) {

  if (!"EVENT" %in% names(packets)) {
    return(parse_packet_set(
      structure(list(), class = "EVENT"),
      NULL, tz, verbose
    ))
  }

  if (verbose) packet_print("startup", "EVENT")

  timestamps <-
    packets$EVENT %>%
    sapply(function(x) x$timestamp) %>%
    anytime::anytime(tz)

  sizes <-
    packets$EVENT %>%
    sapply(function(x) length(x$payload))

  events <-
    packets$EVENT %>%
    sapply(function(x) readBin(
      x$payload, what = "integer", size = 1
    ))

  event_types <-
    ifelse(events == 8, "sleep_ON", "UNKNOWN") %>%
    {ifelse(events == 9, "sleep_OFF", .)} %>%
    factor(c("sleep_ON", "sleep_OFF", "UNKNOWN"))

  set <- data.frame(
    index = NA,
    type = 3,
    timestamp = timestamps,
    payload_size = sizes,
    event_type = event_types
  )

  sleep_check <- grepl("^sleep", event_types)

  if (any(sleep_check)) {

    set <- split(set, sleep_check)

    names(set) %<>%
      gsub("^TRUE$", "idle_sleep_events", .) %>%
      gsub("^FALSE$", "other_events", .)

    set$idle_sleep_events <- format_sleep_events(
      set$idle_sleep_events, info, tz, verbose
    )

    if (!"other_events" %in% names(set)) {
      set$other_events <- data.frame()
      set <- set[c("other_events", "idle_sleep_events")]
    }

  } else {

    set <- list(
      other_events = set, idle_sleep_events = data.frame()
    )

  }

  set_packet_class(set, "EVENT") %T>%
  {if (verbose) packet_print("cleanup", "EVENT")}

}

#' @rdname dev_bin_packets
#' @keywords internal
get_activity <- function(packets, tz, info, verbose) {

  if (!"ACTIVITY" %in% names(packets)) return(NULL)

  if (verbose) packet_print("startup", "ACTIVITY")

  ## Set up timestamps and packet flow

  actual_times <- get_actual(packets$ACTIVITY, tz)

  expected_times <-
    info %$%
    get_expected(Start_Date, Last_Sample_Time, Sample_Rate)

  packet_no <-
    match(expected_times$expected_floor, actual_times, 0) %T>%
    {stopifnot(all(seq(packets$ACTIVITY) %in% .))}

  # ## Complete the processing

  dev_parse_activity(
    packets$ACTIVITY,
    packet_no - 1,
    blank_packet(info$Sample_Rate, .accel_names),
    info$Sample_Rate,
    get_primary_accel_scale(info)
  ) %>%
  data.table::rbindlist(.) %>%
  {data.frame(
    Timestamp = expected_times$expected_full,
    .,
    stringsAsFactors = FALSE,
    row.names = NULL
  )} %>%
  set_packet_class("RAW") %T>%
  {if (verbose) packet_print("cleanup", "ACTIVITY")}

}

#' @rdname dev_bin_packets
#' @keywords internal
get_activity2 <- function(packets, tz, info, verbose) {

  if (!"ACTIVITY2" %in% names(packets)) return(NULL)

  if (verbose) packet_print("startup", "ACTIVITY2")

  ## Set up timestamps and packet flow

    actual_times <- get_actual(packets$ACTIVITY2, tz)

    expected_times <-
      info %$%
      get_expected(Start_Date, Last_Sample_Time, Sample_Rate)

    packet_seq <- seq(packets$ACTIVITY2)

    packet_no <-
      match(expected_times$expected_floor, actual_times, 0) %T>%
      {stopifnot(!anyNA(.))}

    any_discrepancies <-
      setequal(packet_seq, packet_no) %>%
      {!.}

    if (any_discrepancies) {

      ## Okay if it's only one packet at the end of the file
      setdiff(packet_seq, packet_no) %>%
      {stopifnot(
        length(.) == 1,
        . == length(packets$ACTIVITY2)
      )}

    }

  ## Complete the processing

    dev_parse_primary_accelerometerC(
      packets$ACTIVITY2,
      packet_no - 1,
      blank_packet(info$Sample_Rate, .accel_names),
      info$Sample_Rate,
      get_primary_accel_scale(info)
    ) %>%
    data.table::rbindlist(.) %>%
    {data.frame(
      Timestamp = expected_times$expected_full,
      .,
      stringsAsFactors = FALSE,
      row.names = NULL
    )} %>%
    set_packet_class("RAW") %T>%
    {if (verbose) packet_print("cleanup", "ACTIVITY2")}

}

#' @rdname dev_bin_packets
#' @param schema parsed sensor schema information
#' @param parameters parsed parameters information
#' @keywords internal
get_sensor_data <- function(
  packets, schema, parameters, tz, info, verbose
) {

  if (!"SENSOR_DATA" %in% names(packets)) return(NULL)

  if (base::missing(schema)) stop(
    "Cannot parse IMU packets without a sensor schema.\n",
    "  Make sure your call to read_gt3x has (minimally) ",
    " the following:\n  `include = c(\"SENSOR_SCHEMA\",",
    " \"SENSOR_DATA\", \"PARAMETERS\")`", call. = FALSE
  )

  if (verbose) packet_print("startup", "SENSOR_DATA")

  ## Set up timestamps and packet flow

    actual_times <- get_actual(packets$SENSOR_DATA, tz)

    expected_times <-
      actual_times %>%
      {get_expected(
        dplyr::first(.), dplyr::last(.) + 1, schema$samples
      )}

    packet_no <-
      match(expected_times$expected_floor, actual_times, 0) %T>%
      {stopifnot(all(seq(packets$SENSOR_DATA) %in% .))}

    zero_packet <- blank_packet(
      schema$samples, schema$sensorColumns$label
    )

    temperature <- "Temperature" %in% names(zero_packet)
    if (temperature) {
      temp_offset <- get_temp_offset(parameters)
      zero_packet$Temperature %<>% {. - temp_offset}
    }

  ## Complete the processing

    imu <-
      dev_parse_IMU_C(
        packets$SENSOR_DATA,
        packet_no - 1,
        zero_packet,
        schema$id,
        schema$samples,
        schema$sensorColumns
      ) %>%
      data.table::rbindlist(.) %>%
      {data.frame(
        Timestamp = expected_times$expected_full,
        .,
        stringsAsFactors = FALSE,
        row.names = NULL
      )} %>%
      set_packet_class("IMU")

    if (temperature) {
      imu$Temperature %<>% {
        . + get_temp_offset(parameters)
      }
    }

    if (verbose) packet_print("cleanup", "SENSOR_DATA")

    imu

}

#' @rdname dev_bin_packets
#' @keywords internal
get_battery <- function(packets, tz, verbose) {

  if (verbose) packet_print("startup", "BATTERY")

  packets$BATTERY %>%
  lapply(function(x, tz) {
    x$payload %>%
    readBin("integer", 2, 2, FALSE) %>%
    {. / 1000} %>%
    data.frame(
      Timestamp = anytime::anytime(x$timestamp, tz),
      battery_voltage = .
    )
  }, tz = tz) %>%
  c(make.row.names = FALSE) %>%
  do.call(rbind, .) %>%
  set_packet_class("BATTERY") %T>%
  {if (verbose) packet_print("cleanup", "BATTERY")}

}

#' @rdname dev_bin_packets
#' @keywords internal
get_capsense <- function(packets, tz, verbose) {

  if (verbose) packet_print("startup", "CAPSENSE")

  packets$CAPSENSE %>%
  lapply(function(x, tz) {
    data.frame(
      Timestamp = anytime::anytime(x$timestamp, tz),
      capsense_payload(x$payload),
      stringsAsFactors = FALSE
    )
  }, tz = tz) %>%
  c(make.row.names = FALSE) %>%
  do.call(rbind, .) %>%
  set_packet_class("CAPSENSE") %T>%
  {if (verbose) packet_print("cleanup", "CAPSENSE")}

}

#' @rdname dev_bin_packets
#' @keywords internal
get_metadata <- function(packets, tz, verbose) {

  packets$METADATA %>%
  lapply(function(x) x$payload) %>%
  parse_packet_set(
    structure(list(), class = "METADATA"),
    NULL, tz, verbose, .
  ) %T>%
  {if (verbose) packet_print("cleanup", "METADATA")}

}
