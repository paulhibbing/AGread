#' @rdname check_gaps
#' @keywords internal
sleep_latch <- function(object, info, events) {

  events$idle_sleep_events$latch_index <- get_latch_index(
    events$idle_sleep_events$sleep_ON, object$Timestamp
  )

  events$idle_sleep_events <- get_latch_values(
    events$idle_sleep_events, RAW
  )

  sleep_entries <- lapply(
    seq(nrow(events$idle_sleep_events)),
    function(i) {

      latch_index <- events$idle_sleep_events[
        i, "latch_index"
      ]
      latch_value <- object[latch_index, .accel_names]
      latch_times <- seq(
        events$idle_sleep_events$sleep_ON[i],
        events$idle_sleep_events$sleep_OFF[i],
        "1 sec"
      ) %>% {.[!.%in%object$Timestamp]}

      empty_raw(
        latch_times,
        info = info,
        empty_frame = latch_value
      )

    }
  ) %>% data.table::rbindlist(
    .
  ) %>% data.frame(
    ., row.names = NULL
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
