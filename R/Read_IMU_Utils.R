#' Check if the IMU data start on an exact second
#'
#' @param AG a dataframe of IMU data
#'
#' @examples
#' data(imu_to_check)
#' check_second(imu_to_check)
#'
#' @export
check_second <- function(AG) {
  AG$ms <-
    as.numeric(format(AG$Timestamp, "%OS3"))%%1
  if(AG$ms[1]!=0) {
    AG <- AG[-c(1:(which(AG$ms == 0)[1] - 1)),]
  }
  return(AG)
}

#' Get file metadata (sampling frequency, start time, and samples per epoch) for
#' IMU
#'
#' @param file character scalar giving path to IMU file
#' @param output_window_secs the desired epoch length, over which to average IMU
#'   data
#'
#' @examples
#' imu_file <-
#'     system.file("extdata",
#'     "example-IMU.csv",
#'     package = "AGread")
#'
#' get_imu_file_meta(imu_file)
#'
#' @export
get_imu_file_meta <- function(file, output_window_secs = 1) {

  header <- utils::read.csv(
    file,
    nrow = 20,
    stringsAsFactors = FALSE,
    header = FALSE
  )

  samp_rate <- unlist(strsplit(header[, 1], " "))
  samp_rate <- suppressWarnings(
    try(as.numeric(samp_rate[which(samp_rate == "Hz") - 1]))
  )

  if (grepl("error", samp_rate, ignore.case = T)) {
    message_update(21, is_message = TRUE)
    samp_rate = 100
  }

  date_index <- which(
    grepl("start date", header[, 1], ignore.case = T)
  )
  time_index <- which(
    grepl("start time", header[, 1], ignore.case = T)
  )

  start_time <- as.POSIXlt(
    gsub(
      "[[:alpha:] ]",
      "",
      paste(
        header[date_index, 1],
        header[time_index, 1]
      )
    ),
    tz = "UTC",
    format = "%m/%d/%Y%H:%M:%S"
  )

  block_size <- samp_rate * output_window_secs
  return(list(
    start_time = start_time,
    block_size = block_size,
    samp_rate = samp_rate
  ))
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
#' data(imu_to_collapse)
#' imu_collapse(imu_to_collapse, 100)
#'
#' @export
imu_collapse <- function(AG, block_size, verbose = FALSE) {

  ## Establish the epoch labels
  AG <- data.frame(AG, stringsAsFactors = FALSE)

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

  ## Establish the IMU variables in the data and the labels for corresponding
  ## columns
  vars <- sapply(
    c(
      "Accelerometer", "Temperature",
      "Gyroscope", "Magnetometer"
    ),
    function(x) any(grepl(x, names(AG)))
  )

  labels <- c("", "C", "DegPerS", "")[vars]
  absolute <- c(FALSE, FALSE, TRUE, FALSE)[vars]
  vars <- names(vars)[vars]

  ## Store non-IMU variables
  date_processed_IMU <- AG$date_processed_IMU[1]
  file_source_IMU <- AG$file_source_IMU[1]
  timestamps <- AG$Timestamp[diff(c(0, AG$epoch)) != 0]

  ## Collapse the IMU variables
  AG <- mapply(
    imu_var_collapse,
    var = as.list(vars),
    label = as.list(labels),
    absolute = as.list(absolute),
    MoreArgs = list(AG = AG),
    SIMPLIFY = FALSE
  )
  AG <- do.call(cbind, AG)

  ## Re-introduce the non-IMU variables
  AG_names <- names(AG)
  AG$date_processed_IMU <- date_processed_IMU
  AG$file_source_IMU <- file_source_IMU
  AG$Timestamp <- timestamps
  AG <- AG[ ,c("Timestamp", setdiff(names(AG), "Timestamp"))]

  if (verbose) message_update(20)
  AG <- data.frame(AG, stringsAsFactors = FALSE)
  return(AG)
}

#' Collapse an IMU variable
#'
#' @param AG the IMU data frame to select from
#' @param var the character pattern to use in determining which columns to
#'   collapse
#' @param absolute logical. Should columns be converted to absolute values?
#' @param label additional information (i.e., units) to include in variable
#'   names
#'
#' @keywords internal
#'
imu_var_collapse <- function(
  AG, var, absolute = FALSE, label = ""
) {

  if (var != "Magnetometer") {

    AG_vars <- names(AG)[grepl(var, names(AG))]
    if (var == "Gyroscope") AG_vars <- stats::na.omit(
      AG_vars[c(4,1:3)]
    )
    AG <- sapply(
      AG_vars,
      function(x) {

        new_name <- paste0("mean_", x)
        change_name <- !grepl("VM", x)

        if (absolute & change_name) {
          AG[ ,x] <- abs(AG[ ,x])
          new_name <- paste0("mean_abs_", x)
        }

        AG <- AG %>% dplyr::group_by(!!as.name("epoch")) %>%
          dplyr::summarise(
            !!new_name := mean(!!as.name(x))
          )
        AG$epoch <- NULL
        return(AG)
      },
      simplify = FALSE
    )
    AG <- do.call(cbind, AG)
  } else {
    AG <- AG %>% dplyr::group_by(!!as.name("epoch")) %>%
      dplyr::summarise(
        mean_magnetometer_direction = classify_magnetometer(
          mean(!!as.name("Magnetometer.X")),
          mean(!!as.name("Magnetometer.Y")),
          mean(!!as.name("Magnetometer.Z"))
        )
      )
    AG$epoch <- NULL
  }

  names(AG) <- imu_name_label("\\.X$", "_x", label, names(AG))
  names(AG) <- imu_name_label("\\.Y$", "_y", label, names(AG))
  names(AG) <- imu_name_label("\\.Z$", "_z", label, names(AG))
  names(AG) <- gsub(
    "^mean_Temperature$", "mean_Temperature_C", names(AG)
  )

  return(data.frame(AG))

}

#' Fix the column names in IMU data during collapsing
#'
#' @param pattern string pattern to replace
#' @param replacement replacement pattern
#' @param label additional label for the replacement (i.e., units)
#' @param AG_names column names of the IMU data
#'
#' @keywords internal
#'
imu_name_label <- function(pattern, replacement, label, AG_names) {
  new_names <- gsub(
    pattern,
    paste(replacement, label, sep = "_"),
    AG_names
  )
  gsub("_$", "", new_names)
}
