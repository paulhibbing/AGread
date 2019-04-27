#' Fill in accelerometer values for periods when the device was connected to USB
#'
#' @param missing_records logical vector indicating whether a record is labeled
#'   as missing
#' @param records the records to be examined and filled in
#' @inheritParams payload_parse
#'
#' @return
#' @export
#'
#' @examples
latch_accelerometer_records <- function(
  missing_records, records, info
) {

  ## Fix the missing indices per se

    missing_indices <- which(missing_records)
    missing_timestamps <- lapply(
      records[missing_indices],
      function(x) x$Timestamp
    )

    new_records <- lapply(
      missing_timestamps,
      create_zero_record,
      Type = records[[1]]$Type,
      info = info
    )

    records[missing_indices] <- new_records

  ## Now insert zeroes to fill in any gaps

    missing_indices <- missing_indices[
      missing_indices != length(records)
    ] #(Nothing to fill in if it's the last record...)

    n_missing_rows <- sapply(
      missing_indices,
      function(x) difftime(
        records[[x + 1]]$Timestamp,
        records[[x]]$Timestamp,
        units = "sec"
      ) - 1
    )

    # Split based on membership in a missing sequence
    indices <- seq(records)

    missing_runs <- rle(
      indices %in% missing_indices
    )$lengths

    missing_runs <- cumsum(missing_runs)

    group <- sapply(indices, function(x) min(
      which(x <= missing_runs)
      )
    )

    records <- split(records, group)

    index <- match(missing_indices, missing_runs)

    results <- mapply(
      accel_zero_fill,
      index = index, n_missing_rows = n_missing_rows,
      MoreArgs = list(records = records, info = info),
      SIMPLIFY = FALSE, USE.NAMES = FALSE
    )

    records[[index]] <- results[[seq_along(index)]]

    do.call(c, records)

}

#' Correct a single missing ACTIVITY2
#'
#' The correction is made by latching the first half of the missing record to
#' the last sample in the preceding record, then filling the second half of the
#' missing record in with zeroes
#'
#' @param index the index of the missing record
#' @param records the set of all records
#'
#' @keywords internal
#'
accel_latch <- function(index, records) {

  .Defunct(
    msg = paste(
      "The latching process here is most likely useless.",
      "\n  `create_zero_record` should be used instead."
    )
  )

  # index <- missing_indices[1]
  last_value <- records[[index - 1]]$Payload
  last_value <- last_value[nrow(last_value), ]

  current_value <- records[[index]]$Payload

  latch_indices <- seq(
    floor(nrow(current_value) / 2)
  )
  current_value[latch_indices, ] <- last_value

  still_missing_indices <- setdiff(
    seq(nrow(current_value)),
    latch_indices
  )
  current_value[still_missing_indices, ] <- 0

  stopifnot(!anyNA(current_value))

  records[[index]]$Payload <- current_value

  return(records[[index]])

}

#' Fill in gaps in accelerometer data with zeroes
#'
#' @inheritParams accel_latch
#' @param n_missing_rows The number of zero values to fill in
#' @inheritParams payload_parse
#'
#' @keywords internal
#'
accel_zero_fill <- function(
  records, index, n_missing_rows, info, ...
) {

  UseMethod("accel_zero_fill", records)

}

#' @rdname accel_zero_fill
#' @keywords internal
#'
accel_zero_fill.list <- function(
  records, index, n_missing_rows, info, ...
) {

  # index <- index[1]
  old_record <- records[[index]]
  timestamps <- old_record[[1]]$Timestamp + seq(n_missing_rows)

  new_records <- sapply(
    timestamps, create_zero_record,
    Type = old_record[[1]]$Type,
    info = info, simplify = FALSE,
    USE.NAMES = FALSE
  )

  c(old_record, new_records)

}

#' @rdname accel_zero_fill
#' @keywords internal
#'
accel_zero_fill.data.frame <- function(
  records, index, n_missing_rows,
  info, ...
) {

  Type <- switch(
    records$Type[1],
    "ACTIVITY2" = "26"
  )

  stopifnot(!is.null(Type))

  ## Insert zeroes at start ####

    first_time <- records$Timestamp[1]
    tz <- lubridate::tz(first_time)
    start_time <- info$Start_Date
    stopifnot(tz == lubridate::tz(start_time))

    timestamps <- seq(start_time, first_time - 1, "1 sec")

    new_records <- sapply(
      timestamps, create_zero_record,
      Type = Type, info = info,
      simplify = FALSE, USE.NAMES = FALSE
    )

    new_records <- collapse_records(
      new_records, records$Type[1]
    )
    new_records <- post_process(new_records)

    records <- rbind(new_records, records)

    ## Insert zeroes at end ####

    last_time <- records$Timestamp[nrow(records)]
    stop_time <- info$Last_Sample_Time
    stopifnot(tz == lubridate::tz(stop_time))

    timestamps <- seq(last_time + 1, stop_time, "1 sec")

    new_records <- sapply(
      timestamps, create_zero_record,
      Type = Type,
      info = info, simplify = FALSE,
      USE.NAMES = FALSE
    )

    new_records <- collapse_records(
      new_records, records$Type[1]
    )
    new_records <- post_process(new_records)

    rbind(records, new_records)

}
