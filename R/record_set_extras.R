record_set_extras <- function(
  records, label, do_post_process,
  info, sensor_schema, tz, verbose
) {

  if (! label %in% c("ACTIVITY2", "SENSOR_DATA")) {

    records <- collapse_records(records, label)
    records$Timestamp <- lubridate::force_tz(
      records$Timestamp, tz
    )

    if (do_post_process) records <- post_process(records)

    return(records)

  }

  switch(
    label,
    "ACTIVITY2" = ACTIVITY2_extras(
      records, info, tz, label, verbose
    ),
    "SENSOR_DATA" = SENSOR_DATA_extras(records)
  )

}

ACTIVITY2_extras <- function(
  records, info, tz, label, verbose
) {

  missing_records <- sapply(
      records,
      function(x) all(is.na(x$Payload))
  )

  if (any(missing_records)) {

    if (verbose) cat(
      "\r  Parsing", label, "packet(s)",
      "-- interrupting to fill in USB connection time"
    )

    records <- latch_accelerometer_records(
      missing_records, records, info
    )

  }

  if (verbose) cat(
    "\r  Collapsing", label, "packet(s)",
    "  .............                        ",
    "      "
  )

  records <- collapse_records(records, label)
  records <- post_process(records)

  if (verbose) cat(
    "\r  Checking for missing packets",
    "and correcting if necessary            ",
    "      "
  )

  records <- accel_zero_fill(records, info = info)

  records$Timestamp <- timestamp_recalc(
    records$Timestamp, tz,
    verbose, info$Sample_Rate, label
  )

  records

}

SENSOR_DATA_extras <- function(
  records, schema, tz, label, verbose
) {

  records <- interpolate_sensor_records(
    records, schema, verbose
  )

  records <- collapse_records(
    records, label = label
  )
  records <- post_process(records)

  samp_rate <- schema$Payload$samples
  if (samp_rate == 0) samp_rate <- 100


  records$Timestamp <- timestamp_recalc(
    records$Timestamp, tz,
    verbose, samp_rate, label
  )

  records

}
