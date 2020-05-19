#' @rdname read_gt3x_info
#' @keywords internal
info_type1 <- function(file, tz, verbose) {

  info_con <- unz(file$path, "info.txt")
  info <- parse_info_txt(info_con, tz, verbose)
  close(info_con)
  info

}

#' Retrieve information from the gt3x zip archive
#'
#' @inheritParams read_gt3x
#'
#' @keywords internal
read_gt3x_info <- function(file, tz, verbose) {

  if (verbose) cat("\n  Retrieving info from the zip archive")

  file$type %>%
  switch(
    info_type1(file, tz, FALSE)
  ) %T>%
  {if (verbose) cat("  ............. COMPLETE")}

}
