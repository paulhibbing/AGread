fill_imu_records <- function(time_gaps, records, verbose) {

  if (all(time_gaps == 1)) return(records)

  if (verbose) cat(
    "\rResolving time gaps by inserting",
    "zeroes                 "
  )

  gap_indices <- which(time_gaps != 1)

  # Split based on membership in a missing sequence
  indices <- seq(records)

  gap_runs <- rle(
    indices %in% gap_indices
  )$lengths

  gap_runs <- cumsum(gap_runs)

  group <- sapply(
    indices,
    function(x) min(
      which(x <= gap_runs)
    )
  )

  records <- split(records, group)

  index <- match(gap_indices, gap_runs)

  results <- mapply(
    imu_zero_fill,
    index = index,
    MoreArgs = list(records = records),
    SIMPLIFY = FALSE
  )

  records[[index]] <- results[[seq_along(index)]]

  do.call(c, records)

}

#' Fill in missing records for IMU data
#'
#' @inheritParams accel_zero_fill
#'
#' @keywords internal
imu_zero_fill <- function(records, index, ...) {

  UseMethod("imu_zero_fill", records)

}

#' @rdname imu_zero_fill
#'
#' @keywords internal
imu_zero_fill.list <- function(records, index, ...) {

    stopifnot(length(records[[index]]) == 1)
    old_record <- records[[index]][[1]]
    Type <- old_record$Type
    start_time <- old_record$Timestamp

    gap_length <- as.numeric(
      difftime(
        records[[index + 1]][[1]]$Timestamp,
        start_time,
        lubridate::tz(records[[index]]$Timestamp),
        "sec"
      )
    ) - 1

    timestamps <- start_time + seq(gap_length)

    new_records <- lapply(
      timestamps,
      create_zero_record,
      Type = Type,
      info = schema
    )

    c(records[[index]], new_records)

}
