#' Low-Pass filter Gyroscope data
#'
#' @inheritParams check_second
#' @inheritParams
#'
#' @examples
#' \dontrun{
#' data(imu_to_collapse)
#' AGread:::imu_filter_gyroscope(imu)
#' }
#'
#' @keywords internal
imu_filter_gyroscope <- function(AG, samp_rate, filter_hz = 35, verbose = FALSE) {
  if (verbose) message_update(20)
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
