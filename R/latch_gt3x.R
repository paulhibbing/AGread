#' Replace imputed zeroes by carrying forward last non-zero value
#'
#' @param AG data frame of ActiGraph data (could be an \code{activity_df} object
#'   from the \code{read.gt3x} package)
#' @param x_var,y_var,z_var character scalars naming the three columns of raw
#'   acceleration data in \code{AG}
#' @inheritParams read_gt3x
#' @param ... arguments passed to \code{check_missing}
#' @param is_sleep an optional logical vector whose length must equal
#'   \code{nrow(AG)}. Can be passed as an override to internal calculation of
#'   idle sleep via \code{check_missing}
#' @param time_var character scalar naming the column of \code{AG} containing
#'   POSIX-formatted timestamps
#'
#' @note \code{check_missing} may pick up on more than idle sleep mode (e.g.,
#'   USB connect events or trailing zeroes at the end of a file). To exclusively
#'   isolate idle sleep mode, you will need to go through some of the other
#'   \code{AGread::read_gt3x} avenues that include event parsing.
#'
#' @return An updated data frame with latched values rather than zeroes for
#'   missing data
#' @export
#'
#' @examples
#' \donttest{
#' file_3x <- system.file(
#'   "extdata", "example.gt3x", package = "AGread"
#' )
#' AG <- read_gt3x(file_3x)$RAW
#' head(latch_gt3x(AG))
#' }
#'
latch_gt3x <- function(
  AG, x_var = "Accelerometer_X", y_var = "Accelerometer_Y",
  z_var = "Accelerometer_Z", verbose = FALSE,
  flag_idle_sleep = FALSE, ...,
  is_sleep = NULL
){

  stopifnot(inherits(AG, "data.frame"))

  if (is.null(is_sleep)) {
    is_sleep <- check_missing(AG, x_var, y_var, z_var, ...)
  } else {
    stopifnot(length(is_sleep) == nrow(AG))
  }

  if (flag_idle_sleep) AG$idle <- is_sleep else AG$idle <- NULL

  if (!any(is_sleep)) return(AG)

  if (!isTRUE(requireNamespace("zoo", quietly = TRUE))) stop(
    "Please install the `zoo` package to",
    " enable latching of missing gt3x values."
  )

  if (verbose) cat(
    "\n...Latching idle sleep periods"
  )

  AG[is_sleep, c(x_var, y_var, z_var)] <- NA

  if(is_sleep[1]){

    AG[1, c(x_var, y_var, z_var)] <- 0

  }

  AG[[x_var]] <- zoo::na.locf0(AG[[x_var]])
  AG[[y_var]] <- zoo::na.locf0(AG[[y_var]])
  AG[[z_var]] <- zoo::na.locf0(AG[[z_var]])

  AG

}


#' @export
#' @keywords internal
#' @rdname latch_gt3x
check_missing <- function(
  AG, x_var = "Accelerometer_X", y_var = "Accelerometer_Y",
  z_var = "Accelerometer_Z", ...
) {
  UseMethod("check_missing", AG)
}


#' @export
check_missing.default <- function(
  AG, x_var = "Accelerometer_X", y_var = "Accelerometer_Y", z_var = "Accelerometer_Z", ...
) {
  AG[[x_var]]==0 & AG[[y_var]]==0 & AG[[z_var]]==0
}


#' @export
check_missing.activity_df <- function(
  AG, x_var = "Accelerometer_X", y_var = "Accelerometer_Y",
  z_var = "Accelerometer_Z", time_var = "time", ...
) {


  is_sleep <-
    nrow(AG) %>%
    logical()


  if (!exists("missingness", attributes(AG))) {

    NextMethod()

  } else {

    sleep_i <-
      attr(AG, "missingness") %>%
      {mapply(
        function(index, n_missing) index + 1:n_missing - 1,
        index = match(.$time, AG[[time_var]]),
        n_missing = .$n_missing,
        SIMPLIFY = FALSE
      )} %>%
      do.call(c, .) %>%
      {.[. <= nrow(AG)]}

    is_sleep[sleep_i] <- TRUE

    is_sleep

  }


}

