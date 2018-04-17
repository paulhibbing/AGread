#' Low-Pass filter Gyroscope data
#'
#' @inheritParams check_second
#' @inheritParams read_AG_IMU
#'
#' @examples
#' \dontrun{
#' data(imu_to_collapse)
#' imu_filter_gyroscope(imu_to_collapse, 100)
#' }
#'
#' @export
imu_filter_gyroscope <- function(AG, samp_rate, filter_hz = 35, verbose = FALSE) {
  if (verbose) message_update(19, filter_hz = filter_hz)
  AG[, grepl("gyroscope", names(AG), ignore.case = T)] <-
    sapply(AG[, grepl("gyroscope", names(AG),
      ignore.case = T)], function(x) {
        seewave::bwfilter(
          wave = x,
          f = samp_rate,
          n = 2,
          to = filter_hz
        )
      })
  if (verbose) message_update(20)
  return(AG)
}
