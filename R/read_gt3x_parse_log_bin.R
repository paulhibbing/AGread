#' @rdname parse_log_bin
#' @param ... for internal use. Arguments passed to the specific parser from the
#'   \code{parse_log_bin} shell
#' @keywords internal
legacy_parse <- function(file, ...) {
  file$type %>%
  switch(
    legacy_bin_type1(...)
  )
}

#' @rdname parse_log_bin
#' @param ... for internal use. Arguments passed to the specific parser from the
#'   \code{parse_log_bin} shell
#' @keywords internal
dev_parse <- function(file, ...) {
  file$type %>%
  switch(
    dev_bin_type1(...)
  )
}

#' Parse the log component of a gt3x file
#'
#' @param log_file character. Path to the log.bin file
#' @param info result of \code{\link{parse_info_txt}}
#' @inheritParams read_gt3x
#' @param file internal list object containing information about the zip archive
#' @param log RawVector for internal use; the contents of \code{log.bin}
#'
#' @keywords internal
parse_log_bin <- function(
  log_file, info, tz = "UTC", verbose = FALSE,
  include = .packets, parser, file
) {

  ## Validate inputs

    include %<>% validate_include(verbose)
    parser  %<>% validate_parser(.)

  ## Read the bin file

    if (verbose) cat("\n  Reading log.bin")
    log <- readBin(
      log_file, "raw", file$result["log.bin", "Length"]
    )
    if (verbose) cat("  ............. COMPLETE")

  ## Run the desired parser

    switch(

      parser,

      "legacy" = legacy_parse(
        file, log = log, tz = tz, verbose = verbose,
        include = include, info = info
      ),

      "dev" = dev_parse(
        file, log = log, tz = tz, verbose = verbose,
        include = include, info = info
      )

    )

}
