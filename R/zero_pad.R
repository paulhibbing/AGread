#' Fill in missing data packets at start/end of file
#'
#' This function checks whether a packet stream's starting and ending timestamps
#' correspond with what's expected from \code{info.txt}.
#'
#' @inheritParams check_gaps
#'
#' @keywords internal
zero_pad <- function(object, ...) {

  UseMethod("zero_pad", object)

}

#' @rdname zero_pad
#' @export
zero_pad.list <- function(
  object, index, info = NULL, schema = NULL, ...
) {

  stopifnot(length(object[[index]]) == 1)

  old_record <- object[[index]][[1]]

  Type <- old_record$Type

  start_time <- old_record$Timestamp

  gap_length <- as.numeric(
    difftime(
      object[[index + 1]][[1]]$Timestamp,
      start_time,
      lubridate::tz(object[[index]]$Timestamp),
      "sec"
    )
  ) - 1

  timestamps <- start_time + seq(gap_length)

  Info <- switch(
    Type, "25" = schema, "26" = info
  )

  stopifnot(!is.null(Info))

  new_object <- lapply(
    timestamps,
    create_zero_record,
    Type = Type,
    info = Info
  )

  c(object[[index]], new_object)

}

#' @rdname zero_pad
#' @inheritParams check_gaps
#' @param info the content of \code{info.txt}
#' @inheritParams read_gt3x
#' @export
zero_pad.RAW <- function(object, info, verbose, ...) {

  if (verbose) cat(
    "\r  Creating zero pad(s), if",
    "necessary                   "
  )

  ## Background variables ####
    millisecs <- seq(info$Sample_Rate) - 1
    millisecs <- millisecs / info$Sample_Rate
    first_time <- object$Timestamp[1]
    tz <- lubridate::tz(first_time)
    last_time <- object$Timestamp[nrow(object)]

  ## Insert zeroes at start, if necessary ####

    start_time <- info$Start_Date
    stopifnot(tz == lubridate::tz(start_time))

    if (first_time != start_time) {

      timestamps <- seq(
        start_time, first_time - 1, "1 sec"
      )

      start_pad <- sapply(
        timestamps, function(x) {
          data.frame(
            Timestamp = x + millisecs,
            Accelerometer_X = 0,
            Accelerometer_Y = 0,
            Accelerometer_Z = 0
          )
        },
        simplify = FALSE
      )
      start_pad <- do.call(rbind, start_pad)

      object <- rbind(start_pad, object)

    }

  ## Insert zeroes at end ####

    stop_time <- info$Last_Sample_Time - 1
    stopifnot(tz == lubridate::tz(stop_time))

    if (last_time != stop_time) {

      timestamps <- seq(
        lubridate::floor_date(last_time + 1, "second"),
        stop_time, "1 sec"
      )

      end_pad <- sapply(
        timestamps, function(x) {
          data.frame(
            Timestamp = x + millisecs,
            Accelerometer_X = 0,
            Accelerometer_Y = 0,
            Accelerometer_Z = 0
          )
        },
        simplify = FALSE
      )
      end_pad <- do.call(rbind, end_pad)

      object <- rbind(object, end_pad)

    }

  object

}
