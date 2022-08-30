#' File reading function for primary accelerometer files
#'
#' @param file A character scalar giving path to primary accelerometer file
#' @param output_window_secs the desired epoch length; defaults to one second
#' @param calibrate logical. Perform autocalibration using \link[GGIR]{g.calibrate}
#' @param return_raw logical. Return raw triaxial data?
#' @param ... Arguments passed to \code{read.csv} in
#'   \code{\link{check_columns}}
#' @inheritParams read_AG_counts
#' @param block logical. Should file be read in blocks? Will be automatically
#'   invoked if file is larger than 2 GB.
#'
#' @return A dataframe giving processed raw data from the primary accelerometer
#'   in the specified epoch length
#'
#' @examples
#' raw_file <- system.file(
#'   "extdata",
#'   "exampleRAW.csv",
#'   package = "AGread"
#' )
#'
#' ## suppress messages that indicate truncation when sampling
#' ## rate and output window don't line up
#' AG_RAW <- suppressMessages(
#'   read_AG_raw(raw_file)
#' )
#' head(AG_RAW)
#'
#' @export
read_AG_raw <- function(file, output_window_secs = 1,
  calibrate = FALSE, verbose = FALSE, block = FALSE,
  return_raw = FALSE, ...) {

  timer <- proc.time()

  if (verbose) message_update(1, file = file)

  meta <- get_raw_file_meta(file)

  skip <- find_skip(file)

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

      AG$Timestamp <-
        nrow(AG) %>%
        {. - 1} %>%
        rep(1/meta$samp_freq, .) %>%
        cumsum(.) %>%
        c(0, .) %>%
        {meta$start + .}

      AG$file_source_PrimaryAccel <- basename(file)
      AG$date_processed_PrimaryAccel <- Sys.time()

      ordered_names <-
        c(
          "file_source_PrimaryAccel",
          "date_processed_PrimaryAccel",
          "Timestamp"
        ) %>%
        c(., setdiff(names(AG), .)) %>%
        gsub("[. ]+", "_", .)

      AG %<>%
        data.frame(
          stringsAsFactors = FALSE,
          row.names = NULL
        ) %>%
        stats::setNames(., gsub("[. ]+", "_", names(.))) %T>%
        {stopifnot(setequal(names(.), ordered_names))} %>%
        .[, ordered_names]

      if (verbose) message_update(
        16, dur = PAutilities::get_duration(timer)
      )
      return(AG)
    }

    AG <- AG_collapse(AG, output_window_secs, meta$samp_freq)

  }

  AG$file_source_PrimaryAccel <- basename(file)
  AG <- ag_raw_format(AG, meta$start, output_window_secs)

  if (verbose) message_update(
    16, dur = PAutilities::get_duration(timer)
  )

  return(AG)
}
