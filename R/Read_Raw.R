#' File reading function for primary accelerometer files
#'
#' @param file A character scalar giving path to primary accelerometer file
#' @param output_window_secs the desired epoch length; defaults to one second
#' @param calibrate logical. Perform autocalibration using \link[GGIR]{g.calibrate}
#' @param return_raw logical. Return raw triaxial data?
#' @param ... Arguments passed to \code{\link{read.csv}} in
#'   \code{\link{check_columns}}
#' @inheritParams read_AG_counts
#' @param block logical. Should file be read in blocks? Will be automatically
#'   invoked if file is larger than 2 GB.
#'
#' @return A dataframe giving processed raw data from the primary accelerometer
#'   in the specified epoch length
#'
#' @examples
#' raw_file <-
#'     system.file("extdata",
#'     "TestID_LeftWrist_RAW.csv",
#'     package = "AGread")
#'
#' read_AG_raw(raw_file)
#'
#' @export
read_AG_raw <- function(file, output_window_secs = 1,
  calibrate = FALSE, verbose = FALSE, skip = 10, block = FALSE,
  return_raw = FALSE, ...) {

  timer <- proc.time()

  if (verbose) message_update(1, file = file)

  meta <- get_raw_file_meta(file)

  if (any(block, get_file_size__gb(file) > 2)) {

    message("\nReading file in blocks, due to excessive size.")
    AG <- read_AG_raw_block(
        file, output_window_secs, calibrate,
        verbose, skip, meta, timer, ...
    )

  } else {

    raw_data <- check_columns(file, skip = skip, ...)

    if (!raw_data) {
      message_update(17, is_message = TRUE)
      AG <- utils::read.csv(file, stringsAsFactors = FALSE, skip = skip)
    } else {
    AG <- data.table::fread(file, stringsAsFactors = FALSE,
        showProgress = FALSE, skip = skip)
    }

    if (nrow(AG) == 0) {
      message("No data in the file. Returning NULL.")
      return(NULL)
    }

    names(AG) <- gsub("\\.", " ", names(AG))

    if (calibrate) {
      AG <- calibrate_raw(AG, file)
    }

    if (return_raw) {

      AG$Timestamp <- meta$start +
        seq(0, (nrow(AG)-1) / meta$samp_freq, 1 / meta$samp_freq)

      # AG$Block <- NULL

      AG$file_source_PrimaryAccel <- basename(file)
      AG$date_processed_PrimaryAccel <- Sys.time()

      AG$day_of_year <- get_day_of_year(
        AG$Timestamp,
        format = "%Y-%m-%d %H:%M:%S"
      )
      AG$minute_of_day <- get_minute(
        AG$Timestamp,
        format = "%Y-%m-%d %H:%M:%S"
      )

      order <-
        c("file_source_PrimaryAccel",
          "date_processed_PrimaryAccel",
          "Timestamp",
          "day_of_year",
          "minute_of_day")
      AG <- AG[, c(order, setdiff(names(AG), order))]

      if (verbose) message_update(16, dur = get_duration(timer))
      return(AG)
    }

    AG <- AG_collapse(AG, output_window_secs, meta$samp_freq)

  }

  AG$Timestamp <- meta$start +
    seq(0, nrow(AG)-1, output_window_secs)

  AG$Block <- NULL

  AG$file_source_PrimaryAccel <- basename(file)
  AG$date_processed_PrimaryAccel <- Sys.time()

  AG$day_of_year <- get_day_of_year(
    AG$Timestamp,
    format = "%Y-%m-%d %H:%M:%S"
  )
  AG$minute_of_day <- get_minute(
    AG$Timestamp,
    format = "%Y-%m-%d %H:%M:%S"
  )

  order <-
    c("file_source_PrimaryAccel",
      "date_processed_PrimaryAccel",
      "Timestamp",
      "day_of_year",
      "minute_of_day",
      "ENMO")
  AG <- AG[, c(order, setdiff(names(AG), order))]

  if (verbose) message_update(16, dur = get_duration(timer))

  return(AG)
}
