#' @rdname read_gt3x_setup
#' @keywords internal
unzip_gt3x = function(file) {
  if (length(file) == 0) return(file)
  exts = sapply(file, tools::file_ext)
  exts = tolower(exts)
  unzip_these = exts %in% c("gz", "bz", "bz2", "xz")
  # don't decompress if the file doesn't exist
  fe = file.exists(file)
  if (any(unzip_these & fe)) {
    zipped_files = file[unzip_these & fe]
    zip_exts = exts[unzip_these & fe]
    zip_outfiles = mapply(function(x, y) {
      FUN = switch(y,
        bz = bzfile,
        bz2 = bzfile,
        gz = gzfile,
        xz = xzfile)
      R.utils::decompressFile(
        x,
        ext = y,
        FUN = FUN,
        remove = FALSE,
        overwrite = TRUE,
        temporary = TRUE)
    }, zipped_files, zip_exts)
    file[unzip_these & fe] = zip_outfiles
  }
  file
}

#' @rdname read_gt3x_setup
#' @keywords internal
check_gt3x_components <- function(file, verbose = FALSE) {

  if (verbose) cat("\n  Checking components")

  file$Name %>%
  {c(
    all(c("info.txt", "log.bin") %in% .)
  )} %>%
  which(.) %T>%
  {if (!any(.)) stop(
    "read_gt3x is not currently set up to run this type of gt3x",
    " file. Submit an issue on https://github.com/paulhibbing/AGread/issues"
  )} %T>%
  {if (verbose) cat("  ............. COMPLETE")}

}

#' Prepare a gt3x file for parsing
#'
#' @inheritParams read_gt3x
#'
#' @keywords internal
read_gt3x_setup <- function(file, verbose = FALSE) {

  if (verbose) cat("\n  Unzipping")

    file <- unzip_gt3x(file)

    file_3x <- try(
      utils::unzip(file, list = TRUE),
      TRUE
    )

    if ("try-error" %in% class(file_3x)) {
      stop(paste(
        deparse(substitute(file)),
        "is not a valid gt3x file."
      ))
    } else {
      row.names(file_3x) <- file_3x$Name
    }

  if (verbose) cat("  ............. COMPLETE")

  list(
    path = file,
    result = file_3x,
    type = check_gt3x_components(file_3x, verbose)
  )

}
