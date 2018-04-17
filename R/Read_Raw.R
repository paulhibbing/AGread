#' File reading function for primary accelerometer files
#'
#' @param file A character scalar giving path to primary accelerometer file
#' @param output_window_secs the desired epoch length; defaults to one second
#' @inheritParams read_AG_counts
#'
#' @return A dataframe giving processed raw data from the primary accelerometer in the specified epoch length
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
read_AG_raw <- function(file, output_window_secs = 1, verbose = FALSE, skip = 10) {
  timer <- proc.time()

  if (verbose) message_update(1, file = file)

  meta <- get_raw_file_meta(file)

  raw_data <- check_columns(file)
  if (!raw_data) {
    message_update(27, is_message = TRUE)
    AG <- utils::read.csv(file, stringsAsFactors = FALSE, skip = skip)
  } else {
  AG <-
    data.table::fread(file, stringsAsFactors = FALSE, showProgress = FALSE, skip = skip)
  }
  names(AG) <- gsub("\\.", " ", names(AG))

  AG <- AG_collapse(AG, output_window_secs, meta$samp_freq)

  AG$Timestamp <-
    meta$start + seq(0, nrow(AG)-1, output_window_secs)

  AG$Block <- NULL

  AG$file_source_PrimaryAccel <- basename(file)
  AG$date_processed_PrimaryAccel <- Sys.time()

  AG$day_of_year <-
    get_day_of_year(AG$Timestamp, format = "%Y-%m-%d %H:%M:%S")
  AG$minute_of_day <-
    get_minute(AG$Timestamp, format = "%Y-%m-%d %H:%M:%S")

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
