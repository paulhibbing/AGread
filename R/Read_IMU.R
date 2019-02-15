#' File reading function for IMU files
#'
#' @param file character scalar giving the path to the IMU file
#' @param output_window_secs the desired epoch length; defaults to one second
#' @param filter a logical scalar: Apply a low-pass filter to gyroscope data?
#' @param filter_hz The cutoff for the low-pass filter
#' @param output_vars character. IMU variables to include in output.
#' @inheritParams read_AG_counts
#' @inheritParams read_AG_raw
#'
#' @return A dataframe giving processed IMU data in the specified epoch length
#'
#' @examples
#' imu_file <- system.file(
#'   "extdata",
#'   "example-IMU.csv",
#'   package = "AGread"
#' )
#'
#' read_AG_IMU(imu_file)
#'
#' @export
read_AG_IMU <- function(
  file, output_window_secs = 1, verbose = FALSE,
  skip = 10, filter = TRUE, filter_hz = 35,
  output_vars = c(
    "accelerometer", "temperature", "gyroscope", "magnetometer"
  )
) {
  timer <- proc.time()
  if (verbose) message_update(1, file = file)

  ## Establish output variables
  options <- c(
    "accelerometer", "temperature", "gyroscope", "magnetometer"
  )
  output_vars <- match.arg(
    output_vars, c(options, "Error"), TRUE
  )

  ## Carry on with IMU processing
  meta <- get_imu_file_meta(file, output_window_secs)

  AG <-
    suppressWarnings(try(data.table::fread(
      file,
      stringsAsFactors = FALSE,
      skip = skip,
      #nrows = 25,
      showProgress = FALSE
    ))
    )
  if ("try-error" %in% class(AG)) {
    message_update(18, is_message = TRUE)
    return(NULL)
  }
  AG <- data.frame(AG)

  AG$file_source_IMU <- basename(file)
  AG$date_processed_IMU <- Sys.time()
  AG$Timestamp <- meta$start_time +
    (0:(nrow(AG) - 1) / meta$samp_rate)

  AG <- ag_imu_format(
    AG, output_window_secs, filter,
    meta$samp_rate, filter_hz, verbose,
    meta$block_size
  )

  ## Figure out which columns to return
  output_cols <- unlist(sapply(
    output_vars,
    function(x) which(grepl(x, names(AG), ignore.case = TRUE))
  ))
  output_cols <- c(1:3, unname(output_cols))
  output_cols <- output_cols[order(output_cols)]
  output_cols <- output_cols[!duplicated(output_cols)]
  AG <- AG[ ,output_cols]

  if (verbose) message_update(16, dur = get_duration(timer))
  return(AG)
}
