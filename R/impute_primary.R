#' Impute primary accelerometer data for missing packets
#'
#' @param object data frame of primary accelerometer data
#' @inheritParams read_gt3x
#'
#' @keywords internal
impute_primary <- function(object, verbose) {

  if (verbose) cat(
    "\r  Attending to any gaps",
    "(idle sleep mode, USB events, etc)"
  )

  any_gaps <- (!stats::complete.cases(
    object[ ,.accel_names]
  )) %>% {do.call(
    data.frame, rle(.)
  )} %>% {cbind(.,
    stop_index = cumsum(.$lengths) - 1
  )} %>% {cbind(.,
    start_index = .$stop_index - .$lengths + 1
  )} %>% {
    .[.$values, ]
  }

  if (!length(any_gaps)) return(object)

  impute_C(any_gaps, object)

}

#' @rdname impute_primary
#'
#' @inheritParams parse_packet_set.ACTIVITY2
#' @inheritParams read_gt3x
#'
#' @keywords internal
idle_sleep_impute <- function(object, events, info, tz, verbose) {

  if (verbose) cat(
    "\r  Attending to any gaps",
    "(idle sleep mode, USB events, etc)"
  )

  if (nrow(events$idle_sleep_events) == 0) {
    return(object)
  }

  stopifnot(all(
    sign(as.numeric(diff(
      events$idle_sleep_events$sleep_ON
    )))==1
  ))

  latches <- get_latch_index(
    events$idle_sleep_events$sleep_ON,
    object$Timestamp
  ) %>% get_latch_values(
    object
  ) %>% {data.frame(
    run_start = events$idle_sleep_events$sleep_ON,
    run_stop = events$idle_sleep_events$sleep_OFF,
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

  latch_entries <- ifelse(
    latch_entries$Timestamp %in%
      object$Timestamp,
    FALSE,
    TRUE
  ) %>% {
    latch_entries[., ]
  }

  object <- data.table::rbindlist(
    list(object, latch_entries)
  ) %>% data.frame(
    .
  ) %>%
  {.[order(.$Timestamp), ]}

  row.names(object) <- NULL

  object

}
