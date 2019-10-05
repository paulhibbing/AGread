#' Impute IMU data for missing packets
#'
#' @param object data frame of IMU data
#' @inheritParams read_gt3x
#'
#' @keywords internal
impute_IMU <- function(object, verbose) {

  if (verbose) cat(
    "\r  Attending to any gaps in the file"
  )

  imu_names <- setdiff(names(object), "Timestamp")

  any_gaps <- (!stats::complete.cases(
    object[ ,imu_names]
  )) %>% {do.call(
    data.frame, rle(.)
  )} %>% {cbind(.,
    stop_index = cumsum(.$lengths)
  )} %>% {cbind(.,
    start_index = .$stop_index - .$lengths + 1
  )} %>% {
    .[.$values, ]
  }

  if (!length(any_gaps)) return(object)

  gap_indices <- do.call(
    c, mapply(
      seq, from = any_gaps$start_index,
      to = any_gaps$stop_index, SIMPLIFY = FALSE
  ))

  object[gap_indices, imu_names] <- 0

  object

}
