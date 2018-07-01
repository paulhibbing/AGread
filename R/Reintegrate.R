#' Reintegrate a data stream
#'
#' @param ag A data frame to reintegrate
#' @param to The epoch length desired. Starting epoch length will be determined
#'   automatically.
#' @param time_var The name of the column containing POSIX-formatted timestamp
#'   information
#' @param direction The direction of reintegration, i.e. whether a timestamp
#'   refers to the timespan after the previous data point ("backwards"), or
#'   before the next data point ("forwards").
#'
#' @export
#'
#' @examples
#'
#' data("imu_to_check", package = "AGread")
#' ag <-
#'   imu_to_check[ ,c("Timestamp", "mean_abs_Gyroscope_x_DegPerS")]
#'
#' # Forwards reintegration
#'   reintegrate(
#'     ag = ag,
#'     to = 60,
#'     time_var = "Timestamp",
#'     direction = c("forwards")
#'   )
#'
#' # Backwards reintegration
#'   reintegrate(
#'     ag = ag,
#'     to = 60,
#'     time_var = "Timestamp",
#'     direction = c("backwards")
#'   )
#' \dontrun{
#' # Erronious usages that will give a warning
#'   reintegrate(
#'     ag = ag,
#'     to = 60,
#'     time_var = "Timestamp",
#'     direction = c("forwards", "backwards")
#'   )
#'
#'   reintegrate(
#'     ag = ag,
#'     to = 60,
#'     time_var = "Timestamp"
#'   )
#' }
#'
reintegrate <- function(ag, to, time_var = "Timestamp",
  direction = c("forwards", "backwards")) {
  # to <- 60
  # time_var <- "Timestamp"
  # direction <- "backwards"

  direction <- try(
    match.arg(direction, c("forwards", "backwards", "error")),
    silent = TRUE)

  if (class(direction) == "try-error") {
    warning(paste("Argument `direction` must be exactly one of",
      "\"forwards\" or \"backwards\". Defaulting to forwards."))
    direction <- "forwards"
  }

  start_epoch <- unique(diff.POSIXt(ag[ ,time_var]))
  stopifnot(length(start_epoch) == 1, (to / start_epoch) %% 1 == 0)

  block_size <- to / start_epoch
  ag <- get_blocks(ag, time_var, to, start_epoch, block_size, direction)

  col_classes <- sapply(ag, function(x) class(x)[1])
  col_numeric <- col_classes %in% c("numeric", "integer")
  first_vars  <- names(col_classes)[!col_numeric]
  sum_vars    <- names(col_classes)[col_numeric]

  ag[ ,time_var]  <- as.character(ag[ ,time_var])
  firsts  <-
    sapply(match(first_vars, names(ag)), function(x)
      tapply(ag[ ,x], ag$block_no, function(y)
        switch(direction,
          "forwards" = y[1], "backwards" = y[length(y)])))
  firsts <-
    stats::setNames(data.frame(firsts, stringsAsFactors = FALSE),
      first_vars)

  sums    <-
    sapply(match(sum_vars, names(ag)), function(x)
      tapply(ag[ ,x], ag$block_no, sum, na.rm = TRUE))
  sums <- stats::setNames(data.frame(sums, stringsAsFactors = FALSE),
    sum_vars)

  ag <- data.frame(cbind(firsts, sums), stringsAsFactors = FALSE)
  ag[ ,sum_vars] <- sapply(ag[ ,sum_vars], as.numeric)
  ag[ ,time_var] <- as.POSIXct(ag$Timestamp, "UTC")

  ag$block_no <- NULL

  triaxial_vars <- c("Axis1", "Axis2", "Axis3")
  if (all(triaxial_vars %in% names(ag))) {
    ag$Vector.Magnitude <-
      round(get_VM(ag[ ,triaxial_vars]), 2)
  }

  return(ag)
}
