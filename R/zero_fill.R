#' Wrapper for \code{\link{zero_fill}} used to fix data files when the device
#' was connected to USB
#'
#' @inheritParams zero_fill
#' @param run_break_indices indices at which a break occurs
#'
#' @keywords internal
insert_zero_runs <- function(records, run_break_indices) {

  run_break_indices <- run_break_indices[
    run_break_indices != length(records)
  ]

  if (!length(run_break_indices)) return(records)

  indices <- seq(records)

  run_breaks <- rle(
    indices %in% run_break_indices
  )$lengths

  run_breaks <- cumsum(run_breaks)

  group <- sapply(
    indices,
    function(x) min(
      which(x <= run_breaks)
    )
  )

  records <- split(records, group)

  indices <- match(run_break_indices, run_breaks)

  results <- lapply(
    indices, zero_fill, records = records
  )

  records[[indices]] <- results[[seq_along(indices)]]
  do.call(c, records)

}

#' Fill in missing data packets
#'
#' @param records list of parsed packets
#' @param index index of record after which packets are missing
#' @param ... further arguments
#'
#' @keywords internal
zero_fill <- function(records, index, ...) {

  UseMethod("zero_fill", records)

}

#' @rdname zero_fill
#'
#' @keywords internal
zero_fill.list <- function(
  records, index, info = NULL, schema = NULL, ...
) {

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

  Info <- switch(
    Type, "25" = schema, "26" = info
  )

  stopifnot(!is.null(Info))

  new_records <- lapply(
    timestamps,
    create_zero_record,
    Type = Type,
    info = Info
  )

  c(records[[index]], new_records)

}

#' @rdname zero_fill
#' @inheritParams read_record
#'
#' @keywords internal
zero_fill.data.frame <- function(
  records, index, info = NULL,
  schema = NULL, verbose = FALSE, ...
) {

  ## SETUP ####

  Type <- switch(
    records$Type[1],
    "SENSOR_DATA" = "25",
    "ACTIVITY2" = "26"
  )

  if (Type == "25") return(records)

  if (verbose) cat(
    "\r  Checking for missing packets",
    "and correcting if necessary            ",
    "      "
  )

  stopifnot(!is.null(Type))

  ## Insert zeroes at start, if necessary ####

  first_time <- records$Timestamp[1]
  tz <- lubridate::tz(first_time)
  start_time <- info$Start_Date
  stopifnot(tz == lubridate::tz(start_time))

  if (first_time != start_time) {

    timestamps <- seq(
      start_time, first_time - 1, "1 sec"
    )

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

  }

  ## Insert zeroes at end ####

  last_time <- records$Timestamp[nrow(records)]
  stop_time <- info$Last_Sample_Time - 1
  stopifnot(tz == lubridate::tz(stop_time))

  if (last_time != stop_time) {

    timestamps <- seq(
      last_time + 1, stop_time, "1 sec"
    )

    new_records <- sapply(
      timestamps, create_zero_record,
      Type = Type, info = info,
      simplify = FALSE, USE.NAMES = FALSE
    )

    new_records <- collapse_records(
      new_records, records$Type[1]
    )

    new_records <- post_process(new_records)

    records <- rbind(records, new_records)

  }

  records

}
