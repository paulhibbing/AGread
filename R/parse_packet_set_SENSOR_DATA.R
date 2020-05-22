#' @rdname parse_packet_set
#' @param parameters A PARAMETERS object
#' @param schema A SENSOR_SCHEMA object
#' @export
parse_packet_set.SENSOR_DATA <- function(
  set, log, tz = "UTC", verbose = FALSE,
  parameters, schema, ...
) {

  if (is.null(schema)) stop(
    "Cannot parse IMU packets without a sensor schema.\n",
    "  Make sure your call to read_gt3x has (minimally) ",
    " the following:\n  `include = c(\"SENSOR_SCHEMA\",",
    " \"SENSOR_DATA\", \"PARAMETERS\")`", call. = FALSE
  )

  temp_offset <- get_temp_offset(parameters$payload)

  init <-
    set %>%
    {get_times(
      .$timestamp[1],
      .$timestamp[nrow(.)] + 1,
      schema$samples
    )} %>%
    {data.frame(
      Timestamp = lubridate::with_tz(., tz)
    )}

  IMU <-
    set %>%
    legacy_parse_IMU_C(
      log, schema$sensorColumns,
      schema$id, schema$samples,
      verbose
    ) %>%
    data.table::rbindlist(.) %>%
    data.frame(.)

  IMU$Timestamp %<>% lubridate::with_tz(tz)

  if ("Temperature" %in% names(IMU)) {
    if (verbose) cat(
      "\r  Calculating temperature",
      "                                  "
    )
    IMU$Temperature %<>% {. + temp_offset}
  }

  IMU %<>%
    merge(init, ., "Timestamp", all.x = TRUE) %>%
    impute_IMU(., verbose) %>%
    {structure(
      ., class = append(class(.), "IMU", 0)
    )}

  if (verbose) packet_print("cleanup", class(set)[1])

  IMU

}
