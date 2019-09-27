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

  sleeps <- get_latch_values(
    events$idle_sleep_events, RAW
  )

  sleeps <- lapply(
    split(sleeps, seq(nrow(sleeps))),
    function(x) {
      latch_replicate(
        x$sleep_ON, x$sleep_OFF,
        x$Accelerometer_X, x$Accelerometer_Y,
        x$Accelerometer_Z
      )
    }
  ) %>% do.call(rbind, .)

  sleep_entries <- get_latch_entries(
    info$Sample_Rate,
    sleeps$Timestamp,
    sleeps$Accelerometer_X,
    sleeps$Accelerometer_Y,
    sleeps$Accelerometer_Z
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
