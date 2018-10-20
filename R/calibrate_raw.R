#' Perform autocalibration using GGIR package
#'
#' @param AG A dataframe of raw acceleration values
#' @inheritParams read_AG_raw
#'
#' @return A dataframe of calibrated acceleration values
#' @keywords internal
#'
calibrate_raw <- function(AG, file) {

  message_update(29)
  cal_coeffs <- GGIR::g.calibrate(
    datafile = file,
    use.temp = FALSE,
    printsummary = FALSE
  )

  AG <- data.frame(mapply(
    scale,
    x = as.list(AG),
    center = as.list(cal_coeffs$offset),
    scale = as.list(1 / cal_coeffs$scale)
  ))

  names(AG) <- gsub("\\.", " ", names(AG))

  return(AG)
}
