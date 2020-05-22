#' Parse gt3x packets using the dev scheme
#' @rdname dev_bin_packets
#' @param packets list object containing data packets
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
  structure(class = "PARAMETERS") %T>%
  {if (verbose) packet_print("cleanup", "PARAMETERS")}

}

#' @rdname dev_bin_packets
#' @param packets list object containing data packets
#' @inheritParams read_gt3x
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
    log, tz, verbose, .
  ) %T>%
  {if (verbose) packet_print("cleanup", "SENSOR_SCHEMA")}

}

#' @rdname dev_bin_packets
#' @param packets list object containing data packets
#' @inheritParams read_gt3x
#' @keywords internal
get_events <- function(packets, tz, info, verbose) {

  if (!"EVENT" %in% names(packets)) {
    return(parse_packet_set(
      structure(list(), class = "EVENT"),
      log, tz, verbose
    ))
  }

  if (verbose) packet_print("startup", "EVENT")

  timestamps <-
    packets$EVENT %>%
    sapply(function(x) x$timestamp) %>%
    anytime::anytime(tz)

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
    payload_size = 1,
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

  if (verbose) packet_print("cleanup", "EVENT")

  structure(set, class = "EVENT") %T>%
  {if (verbose) packet_print("cleanup", "EVENT")}

}

#' @rdname dev_bin_packets
#' @param all_times vector of all expected timestamps
#' @keywords internal
get_activity2 <- function(packets, all_times, tz, info, verbose) {

  if (!"ACTIVITY2" %in% names(packets)) return(NULL)

  if (verbose) packet_print("startup", "ACTIVITY2")

  packet_no <-
    packets$ACTIVITY2 %>%
    sapply(function(x) x$timestamp) %>%
    anytime::anytime(tz) %>%
    match(all_times, ., 0)

  zero_packet <-
    info$Sample_Rate %>%
    matrix(0, ., 3) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    stats::setNames(.accel_names) %>%
    as.list(.)

  raw <-
    get_primary_accel_scale(info) %>%
    dev_parse_primary_accelerometerC(
      packets$ACTIVITY2, packet_no - 1, zero_packet,
      info$Sample_Rate, .
    ) %>%
    data.table::rbindlist(.)

  times <-
    info %$%
    get_times(
      Start_Date, Last_Sample_Time, Sample_Rate, TRUE
    ) %>%
    lubridate::with_tz(tz) %T>%
    {stopifnot(length(.) == nrow(raw))}

  data.frame(
    Timestamp = times,
    raw,
    stringsAsFactors = FALSE,
    row.names = NULL
  ) %>%
  structure(., class = c("RAW", class(.))) %T>%
  {if (verbose) packet_print("cleanup", "ACTIVITY2")}

}

get_sensor_data <- function(
  packets, schema, all_times, tz, info, verbose
) {

  if (base::missing(schema)) stop(
    "Cannot parse IMU packets without a sensor schema.\n",
    "  Make sure your call to read_gt3x has (minimally) ",
    " the following:\n  `include = c(\"SENSOR_SCHEMA\",",
    " \"SENSOR_DATA\", \"PARAMETERS\")`", call. = FALSE
  )

}
