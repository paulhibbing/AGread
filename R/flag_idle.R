
#' Flag Idle Sleep Times
#'
#' @param RAW packet from \code{\link{parse_log_bin}}
#' @param EVENT packet from \code{\link{parse_log_bin}}
#'
#' @keywords internal
flag_idle <- function(RAW, EVENT) {
  Timestamp = NULL
  rm(list = "Timestamp")
  if (is.null(RAW)) {
    return(NULL)
  }
  if (is.null(EVENT)) {
    stop("need to read in EVENT data as well as ACTIVITY")
  }
  if (NROW(EVENT$idle_sleep_events) == 0) {
    return(RAW)
  }
  sleep_events = EVENT$idle_sleep_events[ ,c("sleep_ON", "sleep_OFF")]
  RAW$TIME = lubridate::floor_date(RAW$Timestamp, "seconds")
  interval_df = mapply(function(x, y) {
    data.frame(
      TIME = seq(from = x,
                 to = y,
                 by = lubridate::as.period(1, "secs")),
      idle = TRUE, stringsAsFactors = FALSE)
  }, sleep_events$sleep_ON, sleep_events$sleep_OFF, SIMPLIFY = FALSE)
  interval_df = dplyr::bind_rows(interval_df)
  interval_df = dplyr::distinct(interval_df)
  RAW = dplyr::full_join(RAW, interval_df, by = "TIME")
  RAW$idle[is.na(RAW$idle)] = FALSE
  RAW = dplyr::arrange(RAW, Timestamp)
  RAW$TIME = NULL
  return(RAW)
}
