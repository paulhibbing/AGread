#' @rdname parse_packet_set
#' @param parameters A PARAMETERS object
#' @param schema A SENSOR_SCHEMA object
#' @export
parse_packet_set.SENSOR_DATA <- function(
  set, log, tz = "UTC", verbose = FALSE,
  parameters, schema, ...
) {

  if (is.null(schema)) stop(paste(
    "Cannot parse IMU packets without",
    "a sensor schema.\n  Make sure your call to read_gt3x",
    " has (minimally) the following:",
    "\n  `include = c(\"SENSOR_SCHEMA\",",
    "\"SENSOR_DATA\", \"PARAMETERS\")`"
  ))

  if (is.null(parameters)) {
    warning(paste(
      "No `parameters` argument passed to ",
      "parse_packet_set.SENSOR_DATA.\n  Assuming",
      "temperature offset is 21 degrees. Make sure by",
      "making a\n  read_gt3x call that has (minimally) the following:",
      "\n  `include = c(\"SENSOR_SCHEMA\", \"SENSOR_DATA\",",
      "\"PARAMETERS\")`"
    ))
    temp_offset <- 21
  } else {

    if (!"IMU_TEMP_OFFSET" %in% names(parameters$Payload)) {
      warning(paste(
        "PARAMETERS object has no `IMU_TEMP_OFFSET` entry.",
        "\n  Defaulting to 21 degrees."
      ))
      temp_offset <- 21
    } else {
      temp_offset <- as.numeric(as.character(
        parameters$Payload$IMU_TEMP_OFFSET
      ))
    }

  }

  init <- get_times(
    set$timestamp[1],
    set$timestamp[nrow(set)] + 1,
    schema$samples
  ) %>% {data.frame(
    Timestamp = lubridate::with_tz(
      ., tz
    )
  )}

  IMU <- parse_IMU_C(
    set, log, schema$sensorColumns,
    schema$id, schema$samples, verbose
  ) %>% {data.frame(
    data.table::rbindlist(.)
  )}

  IMU$Timestamp <- lubridate::with_tz(
    IMU$Timestamp, tz
  )

  if ("Temperature" %in% names(IMU)) {
    if (verbose) cat(
      "\r  Calculating temperature",
      "                                  "
    )
    IMU$Temperature <- IMU$Temperature + temp_offset
  }

  IMU <- merge(
    init, IMU, "Timestamp", all.x = TRUE
  ) %>% impute_IMU(
    ., verbose
  ) %>% {structure(
    ., class = append(class(.), "IMU", 0)
  )}

  if (verbose) packet_print("cleanup", class(set)[1])

  IMU

}
