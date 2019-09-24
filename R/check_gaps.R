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
      object <- sleep_latch(object, tz, info, events)
    }

  ## Fill in latched values for any leftover cases

    missing_times <- get_missing_times(object, tz, info)
    if (!length(missing_times)) return(
      structure(
        object,
        class = unique(append(class(object), "RAW", 0))
      )
    )

    runs <- cumsum(c(1, diff(missing_times)!=1))

    missing_entries <- lapply(
      split(missing_times, runs),
      function(x) {

        latch_index <- get_latch_index(
          x[1], object$Timestamp, tz
        )

        empty_raw(
          x,
          info = info,
          empty_frame = object[
            latch_index, .accel_names
          ]
        )

      }
    ) %>% data.table::rbindlist(
      .
    ) %>% data.frame(.)

    object <- data.table::rbindlist(
      list(object, missing_entries)
      ) %>% data.frame(
        ., row.names = NULL
      ) %>%
      {.[order(.$Timestamp), ]}

    row.names(object) <- NULL

    structure(
      object,
      class = unique(append(class(object), "RAW", 0))
    )

}
