#' Read data from a gt3x file
#'
#' @param file character. Path to the file
#' @param tz character. The timezone to use
#' @param verbose logical. Print updates to console?
#' @param include character. The PACKET types to parse
#' @param flag_idle_sleep should recorded idle sleep times be tagged?
#' @param parser the parsing scheme to use, either \code{dev} or
#'   \code{external}. The external parser is a wrapper for
#'   \code{read.gt3x::read.gt3x}, and specific arguments can be passed in via
#'   \code{...}. The 'Details' section has more information about parsers.
#' @param cleanup logical. Delete unzipped files?
#' @param data_checks logical. Run extra checks on the data (e.g. for large
#'   values and duplicated timestamps)?  Set to \code{FALSE} to speed up
#'   reading.
#' @param ... arguments passed to \code{read.gt3x::read.gt3x} when
#'   \code{parser == "external"}
#'
#' @return A list of processed data, with one element for each of the relevant
#'   packet types.
#'
#' @details
#'
#' Prior to \code{AGread 2.0.0}, a legacy parser was also available. It ran
#' slowly but included extensive checks to ensure alignment with \code{RAW.csv}
#' and \code{IMU.csv} files. The development parser runs faster and has also
#' been checked for alignment with \code{RAW.csv} and \code{IMU.csv} files, but
#' not as strictly. For example, rounding is not performed by
#' \code{parser="dev"}. The legacy parser was removed because of dependency on
#' the \code{binaryLogic} package, which was archived in April 2022.
#'
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
  include =   c("METADATA", "PARAMETERS", "SENSOR_SCHEMA", "BATTERY", "EVENT",
                "TAG", "ACTIVITY", "HEART_RATE_BPM", "HEART_RATE_ANT", "HEART_RATE_BLE",
                "LUX", "CAPSENSE", "EPOCH", "EPOCH2", "EPOCH3", "EPOCH4", "ACTIVITY2",
                "SENSOR_DATA"),
  flag_idle_sleep = FALSE, parser = c("dev", "external"), cleanup = FALSE,
  data_checks = TRUE, ...
) {

  timer <- PAutilities::manage_procedure(
    "Start", "\nProcessing", basename(file), "\n",
    verbose = verbose
  )

  parser <- match.arg(parser)

  if (parser == "legacy") {

    stop(
      "Legacy parser is not available as of AGread 2.0.0.",
      " See ?AGread::read_gt3x",
      call. = FALSE
    )

  } else if (parser == "external") {

    log <- external_parser(file, tz, verbose, ...)

  } else {

    file %<>% read_gt3x_setup(verbose, cleanup)

    info <- read_gt3x_info(file, tz, verbose)

    log  <-
      file$path %>%
      utils::unzip("log.bin", exdir = tempdir()) %>%
      parse_log_bin(info, tz, verbose, include, parser, file)

  }

  if (flag_idle_sleep) {

    if (!all(
      "RAW" %in% names(log),
      "EVENT" %in% names(log)
    )) {

      warning(
        "Cannot flag idle sleep unless both `RAW` and `EVENT` are elements",
        " of the output.\n  Make sure `include` contains \"ACTIVITY\",",
        "\"ACTIVITY2\", and \"EVENT\".",
        "\n  NOTE: `flag_idle_sleep` is not applicable if `parser = 'external'",
        call. = FALSE
      )

    } else {

      log$RAW %<>% flag_idle(log$EVENT, verbose)

    }

  }

  if (!is.null(log$RAW$Timestamp) & data_checks) {

    if (verbose) cat("\n  Running some extra data checks")

    if (anyDuplicated(log$RAW$Timestamp)) warning(
      "Duplicated timestamps in the data. This usually indicates an error",
      call. = FALSE
    )

    if (any(.accel_names %in% names(log$RAW))) {

      intersect(.accel_names, names(log$RAW)) %>%
      log$RAW[ ,.] %>%
      abs(.) %>%
      {. > .odd_value_threshold} %>%
      any(.) %>%
      {if (.) warning(
        "Data values outside of ", .odd_value_threshold,
        " threshold, this usually indicates an error",
        call. = FALSE
      )}

    }

    if (verbose) cat("Checking............. COMPLETE")

  }

  if (cleanup) {

    if (verbose) cat("\n\n  Cleaning up")

    remove_file <- attr(file$path, "remove")

    if (remove_file) {
      file.remove(file$path)
    }

    tempdir() %>%
      file.path("log.bin") %>%
      file.remove(.)

    if (verbose) cat("  ............. COMPLETE")

  }

  PAutilities::manage_procedure(
    "End", "\n\nProcessing complete. Elapsed time",
    PAutilities::get_duration(timer),
    "minutes.\n", verbose = verbose
  )


  return(log)

}
