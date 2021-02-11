#' Set up timestamps for RAW or IMU data frames
#' @rdname get_times
#' @inheritParams get_parameters
#' @inheritParams read_gt3x
#' @keywords internal
get_actual <- function(packets, tz) {

  packets %>%
  sapply("[[", "timestamp") %>%
  anytime::anytime(tz)

}

#' @rdname get_times
#' @param start start time
#' @param end end time
#' @param samp_rate sampling rate
#' @keywords internal
get_expected <- function(start, end, samp_rate) {

  list(
    expected_full = get_seq(start, end, samp_rate),
    expected_floor = get_seq(start, end, 1)
  )

}

#' @rdname get_times
#' @keywords internal
get_seq <- function(start, end, samp_rate) {

  seq(start, end, 1/samp_rate) %>%
  .[-length(.)]

}
