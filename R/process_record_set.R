#' Parse the packets of a certain type
#'
#' @param record_set data frame. The record entries for the packet type.
#' @inheritParams read_record
#' @param verbose logical. Print updates to console?
#' @param do_post_process logical. Divert to \code{\link{post_process}} before
#'   returning?
#'
#' @keywords internal
#'
process_record_set <- function(record_set, log, tz,
  info, give_timestamp, parameters = NULL, schema = NULL,
  verbose = FALSE, do_post_process = TRUE
) {

  # record_set <- record_headers[[6]]
  # record_set <- record_set[1:100, ]

  ## Setup
  n_vals <- nrow(record_set)
  label <- RECORDS$Type[match(
    record_set$type[1],
    as.character(RECORDS$ID)
  )]

  scale_factor <- NULL
  if (label == "ACTIVITY2") {

    scale_factor <- switch(
      substring(info$Serial_Number, 1, 3),
      "NEO" = 341,
      "CLE" = 341,
      "MOS" = 256
    )

    if ("Acceleration_Scale" %in% names(info)) {
      scale_factor <- info$Acceleration_Scale
    }

  }

  if (verbose) cat(
    "\n  Parsing", label, "packet(s)"
  )

  records <- lapply(
    seq(n_vals),
    function(i) {

      if (verbose & (i != n_vals)) cat(
        "\r  Parsing", label, "packet(s)",
        "  .............",
        paste(
          c(round(i/n_vals * 100, 0), "%"),
          collapse = ""
        )
      )

      result <- read_record(
        record_set[i, ], log, tz,
        info, give_timestamp, parameters, schema,
        scale_factor, is_last_packet = i == n_vals
      )

      if (is.null(result$Payload)) return(NULL)
      return(result)
    }
  )

  records[sapply(records, is.null)] <- NULL

  if (verbose) cat(
    "\r  Parsing", label, "packet(s)",
    "-- applying extra processes"
  )

  records <- record_set_extras(
    records, label, do_post_process,
    info, schema, tz, verbose
  )

  if (verbose) cat(
    "\r  Parsing", label, "packet(s)",
    "  ............. COMPLETE               ",
    "      "
  )

  return(records)

}

#' Dispatch-like function for combining packet records of a certain type
#'
#' @param records The records to combine
#' @param label The record type
#' @inheritParams read_gt3x
#'
#' @keywords internal
#'
collapse_records <- function(records, label, verbose = FALSE) {

  if (verbose) cat(
    "\r  Collapsing", label, "packet(s)",
    "  .............                        ",
    "      "
  )

  switch(
    label,
    "METADATA" = generic_record_collapse(records, label),
    "PARAMETERS" = records[[1]], ## Max of one packet already verified
    "SENSOR_SCHEMA" = records[[1]], ## Max of one packet already verified
    "BATTERY" = generic_record_collapse(records, label),
    "EVENT" = generic_record_collapse(records, label),
    "TAG" = list(),
    "ACTIVITY" = list(),
    "HEART_RATE_BPM" = list(),
    "HEART_RATE_ANT" = list(),
    "HEART_RATE_BLE" = list(),
    "LUX" = list(),
    "CAPSENSE" = generic_record_collapse(records, label),
    "EPOCH" = list(),
    "EPOCH2" = list(),
    "EPOCH3" = list(),
    "EPOCH4" = list(),
    "SENSOR_DATA" = generic_record_collapse(records, label),
    "ACTIVITY2" = generic_record_collapse(records, label)
  )

}

#' @rdname collapse_records
#' @keywords internal
generic_record_collapse <- function(records, label) {
  records <- lapply(
    records,
    function(x) data.frame(
      Timestamp = x$Timestamp,
      Type = label,
      Result = x$Payload,
      stringsAsFactors = FALSE
    )
  )
  records <- data.table::rbindlist(records, TRUE)
  # if (!type) records$Type <- NULL
  return(records)
}

#' Apply post-processing to the result of parsing packets of a certain type
#'
#' @param result the result to process
#'
#' @keywords internal
#'
post_process <- function(result) {

  names(result) <- gsub("^Result\\.", "", names(result))
  result <- data.frame(
    result,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (sum(grepl("Timestamp", names(result))) >1) {
    col_to_drop <- which(grepl("Timestamp", names(result)))[1]
    result <- result[ ,-c(col_to_drop)]
    if (sum(grepl("Timestamp", names(result))) == 1) {
      names(result) <- gsub("Timestamp.*", "Timestamp", names(result))
    }
  }

  return(result)

}
