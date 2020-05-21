#' @rdname dev_bin_packets
#' @param packets list object containing data packets
#' @inheritParams read_gt3x
#' @keywords internal
get_parameters <- function(packets, verbose) {

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
get_schema <- function(packets, verbose) {

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
