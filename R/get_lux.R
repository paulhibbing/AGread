.lux_scale <- c(
  "NEO" = NA, "CLE" = NA,
  "MOS0" = 2.05, "MOS2" = 2.05,
  "MRA" = NA, "MOS3" = NA,
  "MOS4" = NA
)

.lux_max <- c(
  "NEO" = NA, "CLE" = NA,
  "MOS0" = 5000, "MOS2" = 5000,
  "MRA" = NA, "MOS3" = NA,
  "MOS4" = NA
)

#' @param lux output from \code{dev_parse_lux}, with minor modifications
#' @param info data frame containing the lux factors
#' @keywords internal
format_lux <- function(lux, info) {

  ifelse(lux$Lux < 20, 0, lux$Lux) %>%
  {. * info$lux_scale} %>%
  round(.) %>%
  {ifelse(
    . > info$lux_max, info$lux_max, .
  )} %>%
  {within(lux, {Lux = .})}

}

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
  format_lux(info) %>%
  set_packet_class("LUX") %T>%
  {if (verbose) packet_print("cleanup", "LUX")}

}
