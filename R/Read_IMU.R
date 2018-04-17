#' File reading function for IMU files
#'
#' @param file character scalar giving the path to the IMU file
#' @param output_window_secs the desired epoch length; defaults to one second
#' @param filter a logical scalar: Apply a low-pass filter to gyroscope data?
#' @param filter_hz The cutoff for the low-pass filter
#' @inheritParams read_AG_counts
#' @inheritParams read_AG_raw
#'
#' @return A dataframe giving processed IMU data in the specified epoch length
#'
#' @examples
#' \dontrun{
#' imu_file <-
#'     system.file("extdata",
#'         "TestID_LeftWrist_IMU.csv",
#'         package = "AGread")
#'
#' read_AG_IMU(imu_file)
#' }
#'
#' @export
read_AG_IMU <- function(file, output_window_secs = 1, verbose = FALSE, skip = 10, filter = TRUE, filter_hz = 35) {
  timer <- proc.time()
  if (verbose) message_update(1, file = file)

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
  AG$Timestamp <- meta$start_time + (0:(nrow(AG) - 1) / meta$samp_rate)

  AG <- check_second(AG)
  if (filter)
    AG <- imu_filter_gyroscope(AG, meta$samp_rate, filter_hz, verbose)

  # Calculate vector magnitudes
  if (verbose) message_update(21)

  AG$mean_Accel_VM <-
    get_VM(AG[, grepl("accelerometer", names(AG), ignore.case = T)], verbose = verbose)

  AG$Gyroscope_VM_DegPerS <-
    get_VM(AG[, grepl("gyroscope", names(AG), ignore.case = T)], verbose = verbose)

  AG$Magnetometer_VM_MicroT <-
    get_VM(AG[, grepl("magnetometer", names(AG), ignore.case = T)], verbose = verbose)

  AG <- imu_collapse(AG, meta$block_size, verbose = verbose)

  first_variables <- c("file_source_IMU", "date_processed_IMU", "Timestamp")

  AG <- AG[, c(first_variables, setdiff(names(AG), first_variables))]

  AG$epoch <- NULL

  if (verbose) message_update(16, dur = get_duration(timer))
  return(AG)
}
