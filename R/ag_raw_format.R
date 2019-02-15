#' Format columns in collapsed raw data
#'
#' @param AG data frame containing raw data
#' @param start_time the start time for calculating timestamps
#' @inheritParams read_AG_raw
#'
#' @keywords internal
#'
ag_raw_format <- function(
  AG, start_time, output_window_secs = 1
) {

  AG$Timestamp <- start_time + seq(
    0, (nrow(AG) * output_window_secs)-1, output_window_secs
  )

  AG$Block <- NULL

  AG$date_processed_PrimaryAccel <- Sys.time()

  AG$day_of_year <- get_day_of_year(
    AG$Timestamp,
    format = "%Y-%m-%d %H:%M:%S"
  )
  AG$minute_of_day <- get_minute(
    AG$Timestamp,
    format = "%Y-%m-%d %H:%M:%S"
  )

  order <- c(
    "file_source_PrimaryAccel",
    "date_processed_PrimaryAccel",
    "Timestamp",
    "day_of_year",
    "minute_of_day",
    "ENMO"
  )
  AG <- AG[, c(order, setdiff(names(AG), order))]

  return(AG)

}
