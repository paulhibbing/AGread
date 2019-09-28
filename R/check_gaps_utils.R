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
RAW_latch <- function(object, run_starts, run_stops, info, tz) {

  latches <- get_latch_index(
    run_starts, object$Timestamp
  ) %>% get_latch_values(
    object
  ) %>% {data.frame(
    run_start = run_starts,
    run_stop = run_stops,
    .
  )}

  latches <- lapply(
    split(latches, seq(nrow(latches))),
    function(x) {
      latch_replicate(
        x$run_start, x$run_stop,
        x$Accelerometer_X, x$Accelerometer_Y,
        x$Accelerometer_Z
      )
    }
  ) %>% do.call(rbind, .)

  latch_entries <- get_latch_entries(
    info$Sample_Rate,
    latches$Timestamp,
    latches$Accelerometer_X,
    latches$Accelerometer_Y,
    latches$Accelerometer_Z
  )

  latch_entries$Timestamp <- lubridate::with_tz(
    latch_entries$Timestamp, tz
  )

  object <- data.table::rbindlist(
    list(object, latch_entries)
  ) %>% data.frame(
    .
  ) %>%
  {.[order(.$Timestamp), ]}

  row.names(object) <- NULL

  object

}
