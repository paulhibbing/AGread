#' Dispatch-like function for parsing different types of payloads
#'
#' @param type The payload type
#' @param payload raw. The payload
#' @inheritParams read_record
#' @param record_header information for
#'   \code{\link{payload_parse_sensor_data_25}} about the packet indices etc.
#' @param is_last_packet logical. Is the current packet the last in the file?
#'
#' @keywords internal
#'
payload_parse <- function(
  type, payload, info, tz = "UTC", parameters = NULL,
  schema = NULL, record_header = NULL, scale_factor,
  is_last_packet = FALSE
) {
  switch(
    type,
    "0" = payload_parse_activity_0(payload, info),
    "2"	= payload_parse_battery_2(payload, info),
    "3"	= payload_parse_event_3(payload, info),
    "4"	= payload_parse_heart_rate_4(payload, info),
    "5"	= payload_parse_lux_5(payload, info),
    "6"	= payload_parse_infodata_6(payload, info),
    "7"	= payload_parse_tag_7(payload, info),
    "9"	= payload_parse_epoch_9(payload, info),
    "11"= payload_parse_heart_ant_11(payload, info),
    "12"=	payload_parse_epoch_12(payload, info),
    "13"= payload_parse_capsense_13(payload, info),
    "14"=	payload_parse_heart_ble_14(payload, info),
    "15"=	payload_parse_epoch_15(payload, info),
    "16"=	payload_parse_epoch_16(payload, info),
    "21"=	payload_parse_parameters_21(payload, info, tz),
    "24"=	payload_parse_sensor_schema_24(payload, info),
    "25"=	payload_parse_sensor_data_25(
      payload, parameters, schema, record_header
    ),
    "26"=	payload_parse_activity2_26(
      payload, info, scale_factor, is_last_packet
    )
  )
}
