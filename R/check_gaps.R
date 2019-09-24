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
  stopifnot(length(tz) == 1)

  ## First handle idle sleep mode, if necessary

    if (nrow(events$idle_sleep_events) != 0) {
      object <- sleep_latch(object, tz, info, events)
    }

  ## Create zeroes for one-byte payloads

    singles <- set[set$payload_size == 1, ]
    if (nrow(singles) > 0) {
      singles$timestamp <- as.POSIXct(
        singles$timestamp, tz
      )
      singles <- empty_raw(singles$timestamp, 0, info)
      object <- data.table::rbindlist(
        list(object, singles)
      ) %>% data.frame(
        ., row.names = NULL
      ) %>%
      {.[order(.$Timestamp), ]}
    }

  ## Next handle trailing zeroes

    object <- trailing_zeroes(object, tz, info)

  ## Fill in values for any leftover missing cases using the
  ## latch-one-then-fill-zeroes approach

    missing_times <- get_missing_times(object, tz, info)
    if (!length(missing_times)) return(
      structure(
        object,
        class = unique(append(class(object), "RAW", 0))
      )
    )

    runs <- cumsum(c(1, diff(missing_times)!=1))

    missing_runs <- data.frame(
      Timestamp = missing_times,
      latch_index = get_latch_index(
        missing_times, object$Timestamp, tz
      ),
      row.names = NULL
    )

    missing_entries <- lapply(
      split(missing_runs, runs),
      function(x) {

        latch_value <- empty_raw(
          x$Timestamp[1],
          info = info,
          empty_frame = object[
            x$latch_index[1], .accel_names
          ]
        )

        zero_values <- empty_raw(
          x$Timestamp[-1], 0, info
        )

        data.table::rbindlist(
          list(latch_value, zero_values)
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
