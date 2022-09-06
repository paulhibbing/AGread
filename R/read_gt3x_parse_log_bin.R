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
#' @param log binary data from \code{log.bin} contents of the \code{gt3x} file
#' @param info result of \code{\link{read_gt3x_info}}
#' @inheritParams read_gt3x
#' @param file internal list object containing information about the zip archive
#' @param log RawVector for internal use; the contents of \code{log.bin}
#'
#' @keywords internal
parse_log_bin <- function(
  log, info, tz = "UTC", verbose = FALSE,
  include = .packets, parser, file
) {

  ## Validate inputs

    include %<>% validate_include(verbose)
    parser  %<>% validate_parser(.)

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
