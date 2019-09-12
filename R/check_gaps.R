#' Examine a packet stream for gaps
#'
#' This function also checks the beginning and end of the stream to see if any
#' padding is necessary based on \code{info.txt}
#'
#' @param object the packet stream
#' @param ... further arguments passed to methods and \code{\link{zero_pad}}
#'
#' @keywords internal
check_gaps <- function(object, ...) {

  UseMethod("check_gaps", object)

}

#' @rdname check_gaps
#' @param info the content of \code{info.txt}
#' @export
check_gaps.RAW <- function(object, info, ...) {

  current_times <- unique(
    lubridate::floor_date(object$Timestamp)
  )

  start_time <- current_times[1]
  end_time <- current_times[length(current_times)]

  expected_times <- seq(start_time, end_time, "1 sec")

  missing_times <- setdiff(
    as.character(expected_times),
    as.character(current_times)
  )

  missing_times <- as.POSIXct(
    missing_times, lubridate::tz(start_time)
  )

  milliseconds <- seq(info$Sample_Rate) - 1
  milliseconds <- milliseconds / info$Sample_Rate
  missing_entries <- sapply(
    missing_times, function(x) {
      data.frame(
        Timestamp = x + milliseconds,
        Accelerometer_X = NA,
        Accelerometer_Y = NA,
        Accelerometer_Z = NA
      )
    },
    simplify = FALSE
  )

  missing_entries <- do.call(rbind, missing_entries)

  object <- rbind(object, missing_entries) %>%
    {.[order(.$Timestamp), ]}

  object <- zero_pad(object, info = info, ...)
  row.names(object) <- NULL

  object

}
