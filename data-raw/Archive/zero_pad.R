#' Fill in missing data packets at start/end of file
#'
#' This function checks whether a packet stream's starting and ending timestamps
#' correspond with what's expected from \code{info.txt}.
#'
#' @inheritParams check_gaps
#'
#' @keywords internal
zero_pad <- function(object, ...) {

  UseMethod("zero_pad", object)

}

#' @rdname zero_pad
#' @export
zero_pad.list <- function(
  object, index, info = NULL, schema = NULL, ...
) {

  stopifnot(length(object[[index]]) == 1)

  old_record <- object[[index]][[1]]

  Type <- old_record$Type

  start_time <- old_record$Timestamp

  gap_length <- as.numeric(
    difftime(
      object[[index + 1]][[1]]$Timestamp,
      start_time,
      lubridate::tz(object[[index]]$Timestamp),
      "sec"
    )
  ) - 1

  timestamps <- start_time + seq(gap_length)

  Info <- switch(
    Type, "25" = schema, "26" = info
  )

  stopifnot(!is.null(Info))

  new_object <- lapply(
    timestamps,
    create_zero_record,
    Type = Type,
    info = Info
  )

  c(object[[index]], new_object)

}
