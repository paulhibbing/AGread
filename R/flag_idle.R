#' Flag Idle Sleep Times
#'
#' @param RAW packet from \code{\link{parse_log_bin}}
#' @param EVENT packet from \code{\link{parse_log_bin}}
#' @inheritParams read_gt3x
#'
#' @keywords internal
flag_idle <- function(RAW, EVENT, verbose = FALSE) {

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

  if (verbose) cat("\n  Labeling idle sleep periods")

  sleep_events <- EVENT$idle_sleep_events[ ,c("sleep_ON", "sleep_OFF")]

  RAW$TIME <- lubridate::floor_date(RAW$Timestamp, "seconds")

  interval_df <-
    mapply(
      function(x, y) {
        data.frame(
          TIME = seq(from = x,
                     to = y,
                     by = lubridate::period("1 sec")),
          idle = TRUE, stringsAsFactors = FALSE)
      },
      sleep_events$sleep_ON,
      sleep_events$sleep_OFF,
      SIMPLIFY = FALSE
    ) %>%
    dplyr::bind_rows(.) %>%
    dplyr::distinct(.)

  RAW <- dplyr::full_join(RAW, interval_df, by = "TIME")

  RAW$idle[is.na(RAW$idle)] <- FALSE
  RAW <- dplyr::arrange(RAW, Timestamp)
  RAW$TIME <- NULL

  if (verbose) cat("  ............. COMPLETE")

  return(RAW)

}
