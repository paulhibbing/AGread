#' Collapse data that were read using \code{\link{read_gt3x}}
#'
#' @param AG The object to collapse, inheriting from class "RAW" or "IMU"
#' @param filename character. Filename to associate with the data.
#' @inheritParams read_AG_IMU
#' @param ... Additional arguments. Currently unused.
#'
#' @return A data frame of collapsed data
#' @export
#'
#' @examples
#' \donttest{
#' file <- system.file(
#'   "extdata",
#'   "example.gt3x",
#'   package = "AGread"
#' )
#' data <- read_gt3x(file)
#' collapse_gt3x(data$RAW)
#' collapse_gt3x(data$IMU)
#' }
collapse_gt3x <- function(
  AG, filename = "gt3x file", output_window_secs = 1,
  filter = TRUE, filter_hz = 35, verbose = FALSE, ...
) {
  UseMethod("collapse_gt3x", AG)
}

#' @rdname collapse_gt3x
#' @method collapse_gt3x RAW
#' @export
collapse_gt3x.RAW <- function(
  AG, filename = "gt3x file", output_window_secs = 1,
  filter = TRUE, filter_hz = 35, verbose = FALSE, ...
) {
  # AG <- test$RAW
  start_time <- AG$Timestamp[1]
  samp_freq <- 1 /
    as.numeric(AG$Timestamp[2] - AG$Timestamp[1])
  samp_freq <- DescTools::RoundTo(samp_freq, 10)
  AG <- AG_collapse(
    AG,
    output_window_secs = output_window_secs,
    samp_freq = samp_freq
  )
  AG$file_source_PrimaryAccel <- filename

  ag_raw_format(
    AG,
    start_time,
    output_window_secs
  )

}

#' @rdname collapse_gt3x
#' @method collapse_gt3x IMU
#' @export
collapse_gt3x.IMU <- function(
  AG, filename = "gt3x file", output_window_secs = 1,
  filter = TRUE, filter_hz = 35, verbose = FALSE,
  ...
) {

  AG$file_source_IMU <- filename
  AG$date_processed_IMU <- Sys.time()
  samp_freq <- 1 /
    as.numeric(AG$Timestamp[2] - AG$Timestamp[1])
  samp_freq <- DescTools::RoundTo(samp_freq, 10)
  block_size <- samp_freq * output_window_secs

  mag_test <-  grepl(
    "Magnetometer", names(AG), ignore.case = TRUE
  )

  if (any(mag_test)) {
    names(AG)[mag_test] <- gsub(
      "_", ".", names(AG)[mag_test]
    )
  }

  AG <- ag_imu_format(
    AG,
    output_window_secs,
    filter,
    samp_freq,
    filter_hz,
    verbose,
    block_size
  )

  names(AG) <- gsub("_X$", "_x", names(AG))
  names(AG) <- gsub("_Y$", "_y", names(AG))
  names(AG) <- gsub("_Z$", "_z", names(AG))

  gyro_test <- grepl(
    "Gyroscope.*[xyz]$", names(AG), ignore.case = TRUE
  )
  if (any(gyro_test)) {
    names(AG)[gyro_test] <- paste(
      names(AG)[gyro_test], "DegPerS", sep = "_"
    )
  }

  return(AG)

}
