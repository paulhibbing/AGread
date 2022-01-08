#' Numerical Minute of the Day.
#'
#' Converts a timestamp to a numerical value between 0 (midnight) and 1439
#' (23:59). Seconds can be represented using a rational decimal.
#'
#' @param timestamp A character or POSIX-formatted vector containing timestamp
#'   information
#' @param format The date-time format of \code{timestamp}, if it is a character
#'   vector
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
get_minute <- function(
  timestamp, format = "%Y-%m-%d %H:%M:%S", rational = FALSE
) {

  check_time(timestamp, format) %>%
  {. - lubridate::floor_date(., "days")} %>%
  as.numeric("mins") %>%
  {. - ((. - floor(.))*!rational)}

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

  check_time(timestamp, format) %>%
  strftime(format = "%j", tz = "UTC") %>%
  as.numeric(.)

}

#' @inheritParams get_minute
#' @keywords internal
check_time <- function(timestamp, format) {

  timestamp %T>%
  {stopifnot(inherits(., c("character", "POSIXt")))} %>%
  {if (is.character(.)) as.POSIXlt(., "UTC", format = format) else .}

}
