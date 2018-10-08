#' Numerical Minute of the Day.
#'
#' Converts a timestamp to a numerical value between 0 (midnight) and 1439
#' (23:59). Seconds can be represented using a rational decimal.
#'
#' @param timestamp A character vector containing timestamp information
#' @param format The date-time format of the \code{timestamp} vector
#' @param rational A logical scalar. Use rational number to represent seconds?
#'
#' @examples
#' key_times <-
#'     paste("2018-03-15",
#'           c("00:00:00",
#'             "01:00:00",
#'             "12:00:00",
#'             "23:59:59"))
#'
#' get_minute(key_times)
#' get_minute(key_times, rational = TRUE)
#'
#' @export
get_minute <- function(timestamp, format = "%Y-%m-%d %H:%M:%S", rational = FALSE) {

    timestamp <- as.POSIXlt(timestamp, format = format, tz = "UTC")

    hour      <-  60 * as.numeric(
      strftime(timestamp, format = "%H", tz = "UTC")
    )
    minute    <- as.numeric(
      strftime(timestamp, format = "%M", tz = "UTC")
    )
    second    <- (1/60) * as.numeric(
      strftime(timestamp, format = "%S", tz = "UTC")
    )

    final_minute <- hour + minute + second
    if(!rational) final_minute <- floor(final_minute)
    return(final_minute)

}


#' Julian Date
#'
#' A wrapper to retrieve the Julian date.
#' @inheritParams get_minute
#'
#' @return A numeric vector of Julian dates.
#'
#' @examples
#' key_dates <- c("2018-01-01", "2018-12-31")
#'
#' get_day_of_year(key_dates, "%Y-%m-%d")
#'
#' @export
get_day_of_year <- function(timestamp, format = "%Y-%m-%d %H:%M:%S") {
    timestamp <- as.POSIXlt(
      timestamp,
      tz = "UTC",
      format = format
    )
    day_of_year <- as.numeric(
      strftime(timestamp, format = "%j", tz = "UTC")
    )
    return(day_of_year)
}
