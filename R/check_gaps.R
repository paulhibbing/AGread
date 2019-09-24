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
check_gaps.RAW <- function(object, info, events, ...) {

  ## Identify all expected timestamps

    current_times <- unique(
      lubridate::floor_date(object$Timestamp, "second")
    )
    tz <- unique(lubridate::tz(current_times))

    start_time <- info$Start_Date
      stopifnot(
        tz == lubridate::tz(start_time),
        start_time %in% current_times
      )

    stop_time <- info$Last_Sample_Time - 1
      stopifnot(tz == lubridate::tz(stop_time))

    expected_times <- seq(start_time, stop_time, "1 sec")

  ## Identify missing timestamps

    missing_times <- setdiff(
      as.character(expected_times), as.character(current_times)
    )
    missing_times <- as.POSIXct(missing_times, tz)

  ## Identify file tail and construct 0g entries

    tail_entries <- NULL
    tail_times <- NULL

    if (stop_time %in% missing_times) {

      tail_times <- rev(missing_times)
      stopifnot(tail_times[1] == stop_time)

      if (length(tail_times) > 1) {
        indices <- as.numeric(diff(tail_times))
        end_index <- which(indices != -1)

        if (!length(end_index)) {
          end_index <- length(end_index)
        } else {
          end_index <- end_index[1]
        }

        tail_times <- rev(tail_times[seq(end_index)])
      }

      tail_entries <- empty_raw(tail_times, 0, info)

      missing_times <- setdiff(
        as.character(missing_times), as.character(tail_times)
      )
      missing_times <- as.POSIXct(missing_times, tz)

    }

  ## Identify missing timestamps and construct NA entries

    missing_entries <- NULL

    if (!!length(missing_times)) {
      missing_entries <- empty_raw(missing_times, NA, info)
    }

  ## Initialize the complete object

    object <- rbind(object, missing_entries, tail_entries) %>%
      {.[order(.$Timestamp), ]}

  ## Populate values for the missing entries via latch or zero insertion

    missing_check <- apply(
      object[ ,.accel_names], 1, function(x) all(is.na(x))
    )

    gaps <- do.call(data.frame, rle(missing_check))
    gap_names <- append(
      names(gaps), c("start_index", "end_index"), 2
    )

    gaps$end_index <- cumsum(gaps$lengths)
    gaps$start_index <- cumsum(
      c(1, gaps$lengths[-nrow(gaps)])
    )

    gaps <- gaps[gaps$values, gap_names]

    for (i in seq(nrow(gaps))) {

      gap_indices <- gaps$start_index[i]:gaps$end_index[i]

      latch_values <- object[
        gaps$start_index[i]-1, .accel_names
      ]

      object[gap_indices, .accel_names] <- latch_values

    }

  row.names(object) <- NULL
  object

}
