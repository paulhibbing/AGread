#' @rdname check_gaps
#' @param tz the timezone
#' @keywords internal
get_missing_times <- function(object, tz, info) {

  ## Identify current timestamps, expected timestamps,
  ## and missing timestamps

  current_times <- unique(lubridate::floor_date(
    object$Timestamp, "second"
  ))
  start_time <- info$Start_Date
  stop_time <- info$Last_Sample_Time - 1

  stopifnot(
    start_time %in% current_times,
    tz == lubridate::tz(start_time),
    tz == lubridate::tz(stop_time)
  )

  expected_times <- seq(
    start_time, stop_time, "1 sec"
  )

  missing_times <- setdiff(
    as.character(expected_times),
    as.character(current_times)
  )

  as.POSIXct(missing_times, tz)

}

#' @rdname check_gaps
#' @keywords internal
sleep_latch <- function(object, info, events, tz) {

  events$idle_sleep_events$latch_index <- get_latch_index(
    events$idle_sleep_events$sleep_ON, object$Timestamp
  )

  events$idle_sleep_events <- get_latch_values(
    events$idle_sleep_events, RAW
  )

  sleep_entries <- get_latch_entries(
    info$Sample_Rate,
    events$idle_sleep_events$sleep_ON,
    events$idle_sleep_events$Accelerometer_X,
    events$idle_sleep_events$Accelerometer_Y,
    events$idle_sleep_events$Accelerometer_Z
  )

  sleep_entries$Timestamp <- lubridate::with_tz(
    sleep_entries$Timestamp, tz
  )

  object <- data.table::rbindlist(
    list(object, sleep_entries)
    ) %>% data.frame(
      .
    ) %>%
    {.[order(.$Timestamp), ]}

  row.names(object) <- NULL

  object

}
