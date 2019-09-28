#' Examine a packet stream for gaps
#'
#' This function also checks the beginning and end of the stream to see if any
#' padding is necessary based on \code{info.txt}
#'
#' @param object the packet stream
#' @param ... further arguments passed to methods
#'
#' @keywords internal
check_gaps <- function(object, ...) {

  UseMethod("check_gaps", object)

}

#' @rdname check_gaps
#' @inheritParams parse_packet_set.ACTIVITY2
#' @export
check_gaps.RAW <- function(object, set, info, events, ...) {

  tz <- unique(lubridate::tz(object$Timestamp))
  stopifnot(
    length(tz) == 1,
    all(sign(as.numeric(diff(object$Timestamp)))==1)
  )

  ## First handle idle sleep mode, if necessary

    if (nrow(events$idle_sleep_events) != 0) {
      stopifnot(all(
        sign(as.numeric(diff(
          events$idle_sleep_events$sleep_ON
        )))==1
      ))
      object <- RAW_latch(
        object,
        events$idle_sleep_events$sleep_ON,
        events$idle_sleep_events$sleep_OFF,
        info,
        tz
      )
    }

  ## Fill in latched values for any leftover cases

    missing_times <- get_missing_times(object, tz, info)
    if (!length(missing_times)) return(
      structure(
        object,
        class = unique(append(class(object), "RAW", 0))
      )
    )

    runs <- cumsum(
      c(1, diff(missing_times)!=1)
    ) %>% {do.call(
      data.frame, rle(.)
    )} %>%{cbind(
      ., stop_index = cumsum(.$lengths)
    )} %>% {cbind(.,
      start_time = missing_times[
        .$stop_index - .$lengths + 1
      ],
      stop_time = missing_times[
        .$stop_index
      ]
    )}

    object <- RAW_latch(
      object, runs$start_time,
      runs$stop_time, info, tz
    )

    row.names(object) <- NULL

    structure(
      object,
      class = unique(append(class(object), "RAW", 0))
    )

}
