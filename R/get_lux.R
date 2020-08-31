#' @rdname dev_bin_packets
#' @inheritParams parse_log_bin
#' @keywords internal
get_lux <- function(packets, tz, info, verbose) {

  if (!"LUX" %in% names(packets)) return(NULL)

  if (verbose) packet_print("startup", "LUX")

  ## Set up timestamps and packet flow

  actual_times <- get_actual(packets$LUX, tz)

  expected_times <- get_seq(
    info$Start_Date, info$Last_Sample_Time, 1
  )

  packet_no <-
    match(expected_times, actual_times, 0) %T>%
    {stopifnot(all(seq(packets$LUX) %in% .))}

  # ## Complete the processing

  dev_parse_lux(
    packets$LUX,
    packet_no - 1,
    blank_packet(1, "Lux")
  ) %>%
  data.table::rbindlist(.) %>%
  {data.frame(
    Timestamp = expected_times,
    .,
    stringsAsFactors = FALSE,
    row.names = NULL
  )} %>%
  set_packet_class("LUX") %T>%
  {if (verbose) packet_print("cleanup", "LUX")}

}
