#' @rdname dev_bin_packets
#' @type character. The packet type
#' @keywords internal
get_default_packet <- function(type) {

  warning(
    "No method exists yet for parsing ",
    type, " packets -- returning NULL."
  )

  return(NULL)

}

#' @rdname dev_bin_packets
#' @keywords internal
dev_bin1_map_packets <- function(
  type, packets, tz, verbose
) {
  switch(
    type,
    "ACTIVITY" = get_default_packet(type),
    "BATTERY" = get_battery(packets, tz, verbose),
    "CAPSENSE" = get_capsense(packets, tz, verbose),
    "EPOCH" = get_default_packet(type),
    "EPOCH2" = get_default_packet(type),
    "EPOCH3" = get_default_packet(type),
    "EPOCH4" = get_default_packet(type),
    "HEART_RATE_ANT" = get_default_packet(type),
    "HEART_RATE_BLE" = get_default_packet(type),
    "HEART_RATE_BPM" = get_default_packet(type),
    "LUX" = get_default_packet(type),
    "METADATA" = get_metadata(packets, tz, verbose),
    "TAG" = get_default_packet(type)
  )
}
