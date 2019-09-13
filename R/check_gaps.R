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

  ## Identify all expected timestamps

    current_times <- unique(
      lubridate::floor_date(object$Timestamp, "second")
    )
    tz <- unique(lubridate::tz(current_times))

    start_time <- info$Start_Date
      stopifnot(tz == lubridate::tz(start_time))

    stop_time <- info$Last_Sample_Time - 1
      stopifnot(tz == lubridate::tz(stop_time))

    expected_times <- seq(start_time, stop_time, "1 sec")

  ## Identify missing timestamps, and (in the original object)
  ## construct NA representation for each one

    missing_times <- setdiff(
      as.character(expected_times),
      as.character(current_times)
    )

    if (!length(missing_times)) return(
      structure(object, row.names = NULL)
    )

    missing_times <- as.POSIXct(missing_times, tz)

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

  ## Populate values for the missing entries via latch or zero insertion

    accel_names <- paste(
      "Accelerometer", c("X","Y","Z"), sep = "_"
    )

    missing_check <- apply(
      object[ ,accel_names], 1, function(x) all(is.na(x))
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

      object[gap_indices, accel_names] <- 0

      latch_indices <- (
        gaps$start_index[i] + seq(info$Sample_Rate) - 1
      )

      latch_values <- object[
        gaps$start_index[i]-1, accel_names
      ]

      object[latch_indices, accel_names] <- latch_values

    }

  row.names(object) <- NULL
  object

}
