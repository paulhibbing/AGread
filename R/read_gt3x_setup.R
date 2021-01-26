#' @rdname read_gt3x_setup
#' @keywords internal
unzip_gt3x <- function(file, cleanup = FALSE) {

  if (length(file) == 0) return(file)
  stopifnot(length(file) == 1)

  exts <- reg_exts <- sapply(file, tools::file_ext)
  exts %<>% tolower(.)

  unzip_these <- exts %in% c("gz", "bz", "bz2", "xz")

  # don't decompress if the file doesn't exist
  fe <- file.exists(file)
  fe_before <-
    paste0("[.]", reg_exts, "$") %>%
    sub("", file) %>%
    file.exists(.)

  if (any(unzip_these & fe)) {

    zipped_files <- file[unzip_these & fe]
    zip_exts <- exts[unzip_these & fe]

    zip_outfiles <- mapply(
      function(x, y) {
        switch(y,
          bz = bzfile,
          bz2 = bzfile,
          gz = gzfile,
          xz = xzfile
        ) %>%
        R.utils::decompressFile(
          x,
          ext = y,
          FUN = .,
          remove = FALSE,
          overwrite = TRUE,
          temporary = TRUE
        )
      },
      zipped_files,
      zip_exts
    )

    file[unzip_these & fe] <- zip_outfiles

  }

  attr(file, "remove") <- unzip_these & cleanup & !fe_before
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
read_gt3x_setup <- function(file, verbose = FALSE, cleanup = FALSE) {

  if (verbose) cat("\n  Unzipping")

    file <- unzip_gt3x(file, cleanup)

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

  if (verbose) cat("  ............. COMPLETE")

  list(
    path = file,
    result = file_3x,
    type = check_gt3x_components(file_3x, verbose)
  )

}
