#' Format idle sleep information from EVENT records
#'
#' @param sleeps data frame with idle sleep mode information
#' @inheritParams read_gt3x
#'
#' @keywords internal
format_sleep_events <- function(sleeps, tz, verbose) {

  if (verbose) cat("\r Formatting idle sleep mode information")

  event_types <- c("sleep_ON", "sleep_OFF")

  stopifnot(
    all(sleeps$event_type %in% event_types),
    length(unique(
      table(droplevels(sleeps$event_type))
    )) == 1,
    sleeps$event_type[1] == "sleep_ON"
  )

  sleeps$pair_num <- cumsum(
    sleeps$event_type == "sleep_ON"
  )

  sleeps$timestamp <- as.character(sleeps$timestamp)
  agg_names <- c("timestamp", "event_type", "pair_num")
  sleeps <- reshape2::dcast(
    sleeps[ ,agg_names],
    ...~event_type,
    value.var = "timestamp"
  ) %>%
    {.[ ,event_types]}

  sleeps$sleep_ON <- as.POSIXct(sleeps$sleep_ON, tz)
  sleeps$sleep_OFF <- as.POSIXct(sleeps$sleep_OFF, tz)

  sleeps$sleep_OFF <- dplyr::if_else(
    sleeps$sleep_OFF == sleeps$sleep_ON,
    sleeps$sleep_OFF, sleeps$sleep_OFF - 1
  )

  sleeps

}
