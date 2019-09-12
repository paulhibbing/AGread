#' Repair packet streams interrupted by USB connection events
#'
#' @param object the sensor stream to repair
#' @param ... further arguments to methods
#'
#' @keywords internal
fix_usb <- function(object, ...) {

  UseMethod("fix_usb", object)

}

#' @rdname fix_usb
#' @param info the content of \code{info.txt}
#' @export
fix_usb.RAW <- function(object, info, ...) {

  accel_names <- paste(
    "Accelerometer", c("X","Y","Z"), sep = "_"
  )

  missing_check <- apply(
    object[ ,accel_names], 1, function(x) all(is.na(x))
  )

  if (!any(missing_check)) return(object)

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

  object

}

#' @rdname fix_usb
#' @export
fix_usb.SENSOR_DATA <- function(records) {

  stop("No updated USB method for IMU packets.")
  time_gaps <- diff(
    sapply(
      records,
      function(x) x$Timestamp,
      USE.NAMES = FALSE
    )
  )

  if (all(time_gaps == 1)) return(records)

  gap_indices <- which(time_gaps != 1)

  insert_zero_runs(records, gap_indices)

}
