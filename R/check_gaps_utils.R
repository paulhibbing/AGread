#' @rdname check_gaps
#' @param missing_times vector of missing timestamps for which to identify a
#'   latch index
#' @param reference_times vector of reference timestamps for use in determining
#'   the latch index
#' @keywords internal
get_latch_index <- function(missing_times, reference_times, tz) {

  mapply(
    difftime,
    time1 = missing_times,
    MoreArgs = list(
      time2 = reference_times,
      tz = tz
    ),
    SIMPLIFY = FALSE
  ) %>% lapply(
    as.numeric
  ) %>% sapply(
    function(x) which(sign(x) < 0)[1] - 1
  )

}

#' @rdname check_gaps
#' @param tz the timezone
#' @keywords internal
sleep_latch <- function(object, tz, info, events) {

  events$idle_sleep_events$latch_index <- get_latch_index(
    events$idle_sleep_events$sleep_ON,
    object$Timestamp,
    tz = tz
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
#' @keywords internal
trailing_zeroes <- function(object, tz, info) {

  ## Identify current stop time and target stop time

    current_stop <- lubridate::floor_date(
      object$Timestamp[nrow(object)], "second"
    )
    target_stop  <- info$Last_Sample_Time - 1

    stopifnot(
      tz == lubridate::tz(target_stop)
    )

  ## Fill missing times (if any) with zeroes

    if (current_stop >= target_stop) return(object)

    zerotimes <- seq(
      current_stop + 1, target_stop, "1 sec"
    )

    padding <- empty_raw(zerotimes, 0, info)

  ## Combine, format, and return

    object <- data.table::rbindlist(
      list(object, padding)
      ) %>% data.frame(
        ., row.names = NULL
      ) %>%
      {.[order(.$Timestamp), ]}

    row.names(object) <- NULL

    object

}

#' @rdname check_gaps
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
