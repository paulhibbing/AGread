#' Check if the primary accelerometer file is formatted correctly
#'
#' \code{check_columns} returns a logical scalar indicating whether there is a
#'  formatting issue with the file passed as the argument. A value of TRUE
#'  indicates the test has passed, whereas FALSE indicates an issue.
#'
#' @inheritParams read_AG_raw
#' @inheritParams read_AG_counts
#' @param ... Arguments passed to \code{\link{read.csv}}
#' @examples
#' raw_file <-
#'     system.file("extdata",
#'     "TestID_LeftWrist_RAW.csv",
#'     package = "AGread")
#'
#' check_columns(raw_file, skip = 10)
#'
#' @export
check_columns <- function(file, skip, ...) {
  test_read <- utils::read.csv(file, nrows = 15, skip = skip, ...)
  if(ncol(test_read) == 1) FALSE else TRUE
}

#' Get file metadata (sampling frequency and timestamps) for primary accelerometer
#'
#' @param file character scalar giving path to primary accelerometer file
#'
#' @examples
#' raw_file <-
#'     system.file("extdata",
#'     "TestID_LeftWrist_RAW.csv",
#'     package = "AGread")
#'
#' get_raw_file_meta(raw_file)
#'
#' @export
get_raw_file_meta <- function(file) {
  file_meta <- data.frame(
    data.table::fread(
      file,
      nrow = 10,
      header = FALSE,
      sep = "\n"
    )
  )
  samp_freq <- file_meta[
    sapply(file_meta, function(x) {
      grepl("Hz", x, ignore.case = T)
    }), ]
  samp_freq <- as.numeric(
    unlist(strsplit(samp_freq, " "))[
      which(grepl(
        "Hz",
        unlist(strsplit(samp_freq, " ")),
        ignore.case = TRUE)
      ) - 1]
  )

  start_time <- gsub(
    "[[:alpha:] ,]",
    "",
    file_meta[
      sapply(file_meta, function(x) {
        grepl("start[. ]time", x,
          ignore.case = T
        )
      }), ]
  )

  start_date <- gsub(
    "[[:alpha:] ,]",
    "",
    file_meta[
      sapply(file_meta, function(x) {
        grepl("start[. ]date", x,
          ignore.case = TRUE
        )
      }), ]
  )

  start <- as.POSIXlt(
    x = paste(start_date, start_time),
    tz = "UTC",
    format = "%m/%d/%Y %H:%M:%S"
  )

  if(is.na(start)) message_update(3, is_message = TRUE)
  return(list(start = start, samp_freq = samp_freq))
}

#' Format columns in collapsed raw data
#'
#' @param AG data frame containing raw data
#' @param start_time the start time for calculating timestamps
#' @inheritParams read_AG_raw
#'
#' @keywords internal
#'
ag_raw_format <- function(
  AG, start_time, output_window_secs = 1
) {

  AG$Timestamp <- start_time + seq(
    0, (nrow(AG) * output_window_secs)-1, output_window_secs
  )

  AG$Block <- NULL

  AG$date_processed_PrimaryAccel <- Sys.time()

  AG$day_of_year <- get_day_of_year(
    AG$Timestamp,
    format = "%Y-%m-%d %H:%M:%S"
  )
  AG$minute_of_day <- get_minute(
    AG$Timestamp,
    format = "%Y-%m-%d %H:%M:%S"
  )

  order <- c(
    "file_source_PrimaryAccel",
    "date_processed_PrimaryAccel",
    "Timestamp",
    "day_of_year",
    "minute_of_day",
    "ENMO"
  )
  AG <- AG[, c(order, setdiff(names(AG), order))]

  return(AG)

}

#' Collapse primary accelerometer data
#'
#' @param AG a dataframe of raw primary accelerometer data
#' @param output_window_secs the desired epoch length; defaults to one second
#' @param samp_freq The sampling frequency
#' @param method character scalar giving the method to use for calculating ENMO,
#'   either "default" or "block"
#' @param ENMO2 vector of leftover raw values from the previous block (if
#'   applicable)
#'
#' @examples
#' data(raw_to_collapse)
#' AG_collapse(raw_to_collapse, 1, 80)
#'
#' @export
AG_collapse <- function(AG, output_window_secs = 1, samp_freq,
  method = "default", ENMO2 = NULL) {

  ## Get ENMO
  ## Adapted from code written by Vincent van Hees

  if (method == "default") {
    AG <- data.frame(AG, stringsAsFactors = FALSE)
    names(AG) <- gsub("\\.", "_", names(AG))

    ENMO <- sqrt(
      AG$Accelerometer_X ^ 2 +
        AG$Accelerometer_Y ^ 2 +
        AG$Accelerometer_Z ^ 2
    ) - 1

  } else {
    ENMO <- sqrt(
      AG$`Accelerometer X` ^ 2 +
        AG$`Accelerometer Y` ^ 2 +
        AG$`Accelerometer Z` ^ 2
    ) - 1
  }

  ENMO[which(ENMO < 0)] <- 0

  if(!is.null(ENMO2)) {

    ENMO2 <- cumsum(c(ENMO2[length(ENMO2)], ENMO))[-1]

  } else {

    ENMO2 <- cumsum(ENMO)

  }

  # Old way of averaging (cuts out one second)
  if (method == "default") {

    indices <- seq(
      1, length(ENMO),
      by = (samp_freq * output_window_secs)
    )

    if (length(ENMO) > max(indices)) message_update(
      24, is_message = TRUE
    )

    ENMO3 <- diff(ENMO2[indices]) /
      (samp_freq * output_window_secs)
    ENMO <- ENMO3 * 1000
    AG <- data.frame(Block = seq(ENMO), ENMO = ENMO)

    # final_length <- min(c(length(ENMO3), nrow(data)))
    # AG <- data.frame(AG$AG[1:final_length, ])
    # ENMO3 <- ENMO3[1:final_length]
    ## /end adapted van Hees code

  }

  if (method == "block") {

    # Applying default method more generally, for processing in blocks
    block <- cumsum(
      seq(length(ENMO)) %% (samp_freq * output_window_secs) == 1
    )

    indices <- seq(
      1, length(ENMO2),
      by = (samp_freq * output_window_secs)
    )
    ENMO <- ENMO2[indices]

    AG <- list(
      ENMO2 = ENMO2[seq(length(ENMO2)) > max(indices)],
      AG = data.frame(Block = seq(ENMO), ENMO = ENMO)
    )

  }


  return(AG)
}

#' Get the size of a file in GB
#'
#' @inheritParams read_AG_raw
#'
#' @keywords internal
#'
get_file_size__gb <- function(file) {

  size  <- structure(
    file.size(file),
    class = "object_size"
  )

  as.numeric(
    gsub(" .*", "", format(size, "GB"))
  )

}
