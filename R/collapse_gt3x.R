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
#' head(collapse_gt3x(data$RAW))
#' head(collapse_gt3x(data$IMU))
#' }
collapse_gt3x <- function(
  AG, filename = "gt3x file", output_window_secs = 1,
  filter = TRUE, filter_hz = 35, verbose = FALSE, ...
) {
  UseMethod("collapse_gt3x", AG)
}

#' @rdname collapse_gt3x
#' @param method the collapsing method to use
#' @export
collapse_gt3x.RAW <- function(
  AG, filename = "gt3x file", output_window_secs = 1,
  filter = TRUE, filter_hz = 35, verbose = FALSE,
  method = c("legacy", "expanded"), ...
) {

  if (verbose) cat(
    "\n...Collapsing primary accelerometer to",
    output_window_secs, "sec windows"
  )

  method <- match.arg(method)
  start_time <- AG$Timestamp[1]
  samp_freq <- 1 /
    as.numeric(AG$Timestamp[2] - AG$Timestamp[1])
  samp_freq <- DescTools::RoundTo(samp_freq, 10)

  if (method == "legacy") {

    AG <- AG_collapse(
      AG,
      output_window_secs = output_window_secs,
      samp_freq = samp_freq
    )
    AG$file_source_PrimaryAccel <- filename

    AG <- ag_raw_format(
      AG,
      start_time,
      output_window_secs
    )

  }

  if (method == "expanded") {

    tz <- lubridate::tz(AG$Timestamp)
    AG$VM <- get_VM(AG[ ,.accel_names], "Rcpp")
    AG$ENMO <- pmax(AG$VM - 1, numeric(nrow(AG)))
    AG$Timestamp <- lubridate::floor_date(
      AG$Timestamp
    ) %>% cut(
      paste(output_window_secs, "sec")
    )
    AG <- AG %>%
      dplyr::group_by(.data$Timestamp) %>%
      dplyr::summarise_all("mean") %>%
      data.frame(
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    AG$Timestamp <- as.POSIXct(
      as.character(AG$Timestamp), tz
    )

  }

  AG

}

#' @rdname collapse_gt3x
#' @method collapse_gt3x IMU
#' @export
collapse_gt3x.IMU <- function(
  AG, filename = "gt3x file", output_window_secs = 1,
  filter = TRUE, filter_hz = 35, verbose = FALSE,
  method = c("legacy", "expanded"), ...
) {

  if (verbose) cat(
    "\n...Collapsing IMU to",
    output_window_secs, "sec windows"
  )

  method <- match.arg(method)

  samp_freq <- 1 /
    as.numeric(AG$Timestamp[2] - AG$Timestamp[1])
  samp_freq <- DescTools::RoundTo(samp_freq, 10)
  block_size <- samp_freq * output_window_secs

  if (method == "legacy") {

    AG$file_source_IMU <- filename
    AG$date_processed_IMU <- Sys.time()

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

  }

  if (method == "expanded") {

    tz <- lubridate::tz(AG$Timestamp)

    if (all(.accel_names %in% names(AG))) {
      AG <- AG_insert(
        AG, "VM", "Accelerometer_Z",
        get_VM(AG[ ,.accel_names], "Rcpp")
      ) %>% {AG_insert(
        ., "ENMO", "VM",
        pmax(.$VM - 1, numeric(nrow(.)))
      )}
    }

    if (all(.gyro_names %in% names(AG))) {
      if (filter) {
        AG <- imu_filter_gyroscope(
          AG, samp_freq, filter_hz, verbose
        )
      }
      AG <- AG_insert(
        AG, "GVM", "Gyroscope_Z",
        get_VM(AG[ ,.gyro_names], "Rcpp")
      )
    }

    AG$Timestamp <- lubridate::floor_date(
      AG$Timestamp
    ) %>% cut(
      paste(output_window_secs, "sec")
    )

    AG <- AG %>%
      dplyr::group_by(.data$Timestamp) %>%
      dplyr::summarise_all("mean") %>%
      data.frame(
        stringsAsFactors = FALSE,
        row.names = NULL
      )

    AG$Timestamp <- as.POSIXct(
      as.character(AG$Timestamp), tz
    )

    if (all(.mag_names %in% names(AG))) {
      AG <- AG_insert(
        AG, "Direction_Vertical", "Magnetometer_Z",
        apply(
          AG[ ,.mag_names], 1,
          function(x) classify_magnetometer(
            x[1], x[2], x[3]
          )
        )
      ) %>% {AG_insert(
        ., "Direction_Horizontal", "Direction_Vertical",
        apply(
          AG[ ,.mag_names], 1,
          function(y) classify_magnetometer(
            y[1], y[2], y[3], "horizontal"
          )
        )
      )}
    }

  }

  AG

}
