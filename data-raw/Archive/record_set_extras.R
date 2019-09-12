record_set_extras <- function(
  records, label, do_post_process,
  info, sensor_schema, tz, verbose = FALSE
) {

  if (! label %in% c("ACTIVITY2", "SENSOR_DATA")) {

    records <- collapse_records(records, label, verbose)
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
    "SENSOR_DATA" = SENSOR_DATA_extras(
      records, sensor_schema, tz, label, verbose
    )
  )

}

ACTIVITY2_extras <- function(
  records, info, tz, label,
  verbose = FALSE
) {

  records <- fix_usb(
    Type = records[[1]]$Type,
    verbose = verbose,
    records = records,
    info = info
  )
  records <- collapse_records(
    records, label, verbose
  )
  records <- post_process(records)
  records <- zero_fill(
    records, info = info, verbose = verbose
  )

  records$Timestamp <- timestamp_recalc(
    records$Timestamp, tz,
    verbose, info$Sample_Rate, label
  )

  records

}

SENSOR_DATA_extras <- function(
  records, schema, tz, label, verbose
) {

  records <- fix_usb(
    Type = records[[1]]$Type,
    verbose = verbose,
    records = records
  )

  records <- collapse_records(
    records, label, verbose
  )

  # records <- interpolate_sensor_records(
  #   records, schema, verbose
  # )

  records <- post_process(records)

  records$Timestamp <- timestamp_recalc(
    records$Timestamp, tz,
    verbose, schema$Payload$samples, label
  )

  records

}
