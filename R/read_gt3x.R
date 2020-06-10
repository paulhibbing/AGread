#' Read data from a gt3x file
#'
#' @param file character. Path to the file
#' @param tz character. The timezone to use
#' @param verbose logical. Print updates to console?
#' @param include character. The PACKET types to parse
#' @param flag_idle_sleep should recorded idle sleep times be tagged?
#' @param cleanup should any unzipped files be deleted?
#'
#' @return A list of processed data, with one element for each of the relevant
#'   packet types.
#'
#' @details
#' The default value for \code{include} gives all possible packet types, of
#' which there are 18. Processing time can be reduced by passing a subset of the
#' 18 possibilities. Exclusion is not recommended for the PARAMETERS and
#' SENSOR_SCHEMA packets, which also do not take long to process.
#'
#' @references
#' \url{https://github.com/actigraph/GT3X-File-Format}
#'
#' @export
#'
#' @examples
#' \donttest{
#' file_3x <- system.file(
#'   "extdata", "example.gt3x", package = "AGread"
#' )
#' AG_3x <- read_gt3x(file_3x)
#' head(lapply(AG_3x, head))
#' }
#'
read_gt3x <- function(
  file, tz = "UTC", verbose = FALSE,
  flag_idle_sleep = FALSE,
  include =   c("METADATA", "PARAMETERS", "SENSOR_SCHEMA", "BATTERY", "EVENT",
                "TAG", "ACTIVITY", "HEART_RATE_BPM", "HEART_RATE_ANT", "HEART_RATE_BLE",
                "LUX", "CAPSENSE", "EPOCH", "EPOCH2", "EPOCH3", "EPOCH4", "ACTIVITY2",
                "SENSOR_DATA"),
  cleanup = FALSE
) {

  timer <- PAutilities::manage_procedure(
    "Start", "\nProcessing", basename(file), "\n",
    verbose = verbose
  )

  #1) Verify .gt3x file is a zip file

  file <- unzip_zipped_gt3x(file, cleanup = cleanup)
  remove_file = attr(file, "remove")

    file_3x <- try(
      utils::unzip(file, list = TRUE),
      TRUE
    )

    if (inherits(file_3x, "try-error")) {
      stop(paste(
        deparse(substitute(file)),
        "is not a valid gt3x file."
      ))
    } else {
      row.names(file_3x) <- file_3x$Name
    }

  #2) Verify .gt3x file has log.bin file
  #3) Verify .gt3x file has info.txt file

    stopifnot(all(c("info.txt", "log.bin") %in% file_3x$Name))

  #4) Extract info.txt

    info_con <- unz(file, "info.txt")

  #5) Parse and save the sample rate from the info.txt file (it's stored in Hz)
  #6) Parse and save the start date from the info.txt file (it's stored in .NET
  #Ticks)

    info <- parse_info_txt(info_con, tz, verbose)
    close(info_con)

  #7) Extract log.bin
  #8) Parse log.bin

    log_file  <- utils::unzip(file, "log.bin", exdir = tempdir())
    log  <- parse_log_bin(
      log_file, file_3x["log.bin", "Length"], info, tz,
      verbose, include
    )

    if (flag_idle_sleep) {
      # if (!all(c("RAW", "EVENT") %in% names(log))) {
      #   warning(paste0("flag_idle_sleep = TRUE, but RAW and EVENT",
      #                  " were not included in choices or were NULL, ",
      #                  "skipping"))
      # }
      log$RAW = flag_idle(log$RAW, log$EVENT)
    }
  PAutilities::manage_procedure(
    "End", "\n\nProcessing complete. Elapsed time",
    PAutilities::get_duration(timer),
    "minutes.\n", verbose = verbose
  )
  if (cleanup) {
    if (remove_file) {
      file.remove(file)
    }
    file.remove(file.path(tempdir(), "log.bin"))
  }

  return(log)

}

