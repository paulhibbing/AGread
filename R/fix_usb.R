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
