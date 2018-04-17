#' Check if the primary accelerometer file is formatted correctly
#'
#' \code{check_columns} returns a logical scalar indicating whether there is a
#'  formatting issue with the file passed as the argument. A value of TRUE
#'  indicates the test has passed, whereas FALSE indicates an issue.
#'
#' @inheritParams read_AG_raw
#' @examples
#' \dontrun{
#' raw_file <-
#'     system.file("extdata",
#'     "TestID_LeftWrist_RAW.csv",
#'     package = "AGread")
#'
#' AGread:::check_columns(raw_file)
#' }
#'
#' @keywords internal
check_columns <- function(file) {
  test_read <- utils::read.csv(file, nrows = 15)
  if(ncol(test_read) == 1) FALSE else TRUE
}

#' Check if the IMU data start on an exact second
#'
#' @param AG a dataframe of IMU data
#'
#' @examples
#' \dontrun{
#' data(imu_to_check)
#' AGread:::check_second(imu_to_check)
#' }
#'
#' @keywords internal
check_second <- function(AG) {
  AG$ms <-
    as.numeric(format(AG$Timestamp, "%OS3"))%%1
  if(AG$ms[1]!=0) {
    AG <- AG[-c(1:(which(AG$ms == 0)[1] - 1)),]
  }
  return(AG)
}

#' Get file metadata (sampling frequency and timestamps) for primary accelerometer
#'
#' @param file character scalar giving path to primary accelerometer file
#'
#' @examples
#' \dontrun{
#' raw_file <-
#'     system.file("extdata",
#'     "TestID_LeftWrist_RAW.csv",
#'     package = "AGread")
#'
#' AGread:::get_raw_file_meta(raw_file)
#' }
#'
#' @keywords internal
get_raw_file_meta <- function(file) {
  file_meta <-
    data.frame(data.table::fread(
      file,
      nrow = 10,
      header = F,
      sep = "\n"
    ))
  samp_freq <-
    file_meta[sapply(file_meta, function(x) {
      grepl("Hz", x, ignore.case = T)
    }),]
  samp_freq <-
    as.numeric(unlist(strsplit(samp_freq, " "))[which(grepl("Hz", unlist(strsplit(samp_freq, " ")), ignore.case = T)) - 1])

  start_time <-
    gsub("[[:alpha:] ,]", "", file_meta[sapply(file_meta, function(x) {
      grepl("start[. ]time", x,
        ignore.case = T)
    }), ]
    )
  start_date <-
    gsub("[[:alpha:] ,]", "", file_meta[sapply(file_meta, function(x) {
      grepl("start[. ]date", x,
        ignore.case = T)
    }), ])
  start <-
    as.POSIXlt(paste(start_date, start_time), format = "%m/%d/%Y %H:%M:%S")
  if(is.na(start)) message_update(3, is_message = TRUE)
  return(list(start = start, samp_freq = samp_freq))
}

#' Get file metadata (sampling frequency, start time, and samples per epoch) for primary accelerometer
#'
#' @param file character scalar giving path to IMU file
#' @param output_window_secs the desired epoch length, over which to average IMU data
#'
#' @examples
#' \dontrun{
#' imu_file <-
#'     system.file("extdata",
#'     "TestID_LeftWrist_IMU.csv",
#'     package = "AGread")
#'
#' AGread:::get_imu_file_meta(imu_file, 1)
#' }
#'
#' @keywords internal
get_imu_file_meta <- function(file, output_window_secs = 1) {
  header <-
    utils::read.csv(file, nrow = 20, stringsAsFactors = F, header = F)

  samp_rate <- unlist(strsplit(header[, 1], " "))
  samp_rate <-
    suppressWarnings(try(as.numeric(samp_rate[which(samp_rate == "Hz") - 1])))
  if (grepl("error", samp_rate, ignore.case = T)) {
    message_update(21, is_message = TRUE)
    samp_rate = 100
  }

  date_index <-
    which(grepl("start date", header[, 1], ignore.case = T))
  time_index <-
    which(grepl("start time", header[, 1], ignore.case = T))

  start_time <-
    as.POSIXlt(gsub("[[:alpha:] ]", "",
      paste(header[date_index, 1],
        header[time_index, 1])),
      format = "%m/%d/%Y%H:%M:%S")

  block_size <- samp_rate * output_window_secs
  return(list(
    start_time = start_time,
    block_size = block_size,
    samp_rate = samp_rate
  ))
}

#' Collapse primary accelerometer data
#'
#' @param AG a dataframe of raw primary accelerometer data
#' @param output_window_secs the desired epoch length; defaults to one second
#' @param samp_freq The sampling frequency
#'
#' @examples
#' \dontrun{
#' data(raw_to_collapse)
#' AGread:::AG_collapse(raw_to_collapse, 1, 80)
#' }
#'
#' @keywords internal
AG_collapse <- function(AG, output_window_secs = 1, samp_freq) {
  ## Get ENMO
  ## Adapted from code written by Vincent van Hees
  ENMO <-
    sqrt(AG$`Accelerometer X` ^ 2 + AG$`Accelerometer Y` ^ 2 + AG$`Accelerometer Z` ^
        2) - 1
  ENMO[which(ENMO < 0)] <- 0
  ENMO2 <- cumsum(ENMO)
  ENMO3 <-
    diff(ENMO2[seq(1, length(ENMO), by = (samp_freq * output_window_secs))]) /
    (samp_freq * output_window_secs)

  # final_length <- min(c(length(ENMO3), nrow(data)))
  # AG <- data.frame(AG$AG[1:final_length, ])
  # ENMO3 <- ENMO3[1:final_length]
  ENMO <- ENMO3 * 1000
  ## /end adapted van Hees code

  AG <- data.frame(Block = seq(ENMO), ENMO = ENMO)
  return(AG)
}

#' Collapse raw IMU data to a specified epoch
#'
#' @param AG dataframe containing raw IMU data
#' @param block_size number of samples per epoch
#' @inheritParams read_AG_counts
#'
#' @return dataframe of IMU data averaged over the specified epoch length
#'
#' @examples
#' \dontrun{
#' data(imu_to_collapse)
#' AGread:::imu_collapse(imu_to_collapse, 100)
#' }
#'
#' @keywords internal
imu_collapse <- function(AG, block_size, verbose = FALSE) {

  if (nrow(AG) %% block_size != 0) {
    message_update(24, is_message = TRUE)
    final_obs <-
      rev(seq_len(nrow(AG)))[which(rev(seq_len(nrow(AG)))[1:block_size] %%
          (block_size) ==
          0)[1]]
    AG <- AG[1:final_obs, ]
  }

  AG$epoch <- rep(1:(nrow(AG) / block_size), each = block_size)

  if (verbose) message_update(25)
  AG <-
    AG %>% dplyr::group_by_(~epoch) %>%
    dplyr::summarise_(
      date_processed_IMU = ~date_processed_IMU[1],
      file_source_IMU = ~file_source_IMU[1],
      Timestamp = ~Timestamp[1],
      #Time = ~Time,
      Gyroscope_VM_DegPerS = ~mean(Gyroscope_VM_DegPerS),
      mean_abs_Gyroscope_x_DegPerS = ~mean(abs(Gyroscope.X)),
      mean_abs_Gyroscope_y_DegPerS = ~mean(abs(Gyroscope.Y)),
      mean_abs_Gyroscope_z_DegPerS = ~mean(abs(Gyroscope.Z)),
      mean_magnetometer_direction = ~classify_magnetometer(
        mean(Magnetometer.X),
        mean(Magnetometer.Y),
        mean(Magnetometer.Z)
      )
    )

  if (verbose) message_update(20)
  AG <- as.data.frame(AG)
  return(AG)
}
