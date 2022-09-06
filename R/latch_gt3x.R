#' Replace imputed zeroes by carrying forward last non-zero value
#'
#' @param AG data frame of ActiGraph data (could be an \code{activity_df} object
#'   from the \code{read.gt3x} package)
#' @param x_var,y_var,z_var character scalars naming the three columns of raw
#'   acceleration data in \code{AG}
#' @inheritParams read_gt3x
#' @param ... arguments passed to \code{get_sleep}
#' @param time_var character scalar naming the column of \code{AG} containing
#'   POSIX-formatted timestamps
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
#' head(latch_gt3x(AG, "Accelerometer_X", "Accelerometer_Y", "Accelerometer_Z"))
#' }
#'
latch_gt3x <- function(
  AG, x_var = "X", y_var = "Y",
  z_var = "Z", verbose = FALSE,
  flag_idle_sleep = FALSE, ...
){
browser()
  is_sleep <- get_sleep(AG, x_var, y_var, z_var, ...)

  if (flag_idle_sleep) AG$idle <- is_sleep

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
get_sleep <- function(
  AG, x_var = "X", y_var = "Y",
  z_var = "Z", ...
) {
  UseMethod("get_sleep", AG)
}


#' @export
get_sleep.default <- function(
  AG, x_var = "X", y_var = "Y", z_var = "Z", ...
) {
  AG[[x_var]]==0 & AG[[y_var]]==0 & AG[[z_var]]==0
}


#' @export
get_sleep.activity_df <- function(
  AG, x_var = "X", y_var = "Y",
  z_var = "Z", time_var = "time", ...
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

