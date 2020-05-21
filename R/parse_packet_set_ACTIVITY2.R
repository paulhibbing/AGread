#' @rdname parse_packet_set
#' @param info the result of \code{\link{parse_info_txt}}
#' @param events the result of parsing EVENTS packets
#' @param raw_method the method to use for formatting the ACTIVITY2 packets
#'   (internal use)
#' @export
parse_packet_set.ACTIVITY2 <- function(
  set, log, tz = "UTC", verbose = FALSE,
  info, events, raw_method, ...
) {

  stopifnot(raw_method %in% 1:2)
  init <- init_RAW(info)

  RAW <- switch(
    raw_method,
    get_RAW1(info, set, log, verbose),
    get_RAW2(info, set, verbose)
  )

  RAW$Timestamp %<>% lubridate::with_tz(tz)

  RAW %<>%
    idle_sleep_impute(events, info, tz, verbose) %>%
    merge(init, ., "Timestamp", all.x = TRUE) %>%
    impute_primary(verbose) %>%
    {structure(
      ., class = append(class(.), "RAW", 0)
    )}

  if (verbose) packet_print("cleanup", class(set)[1])

  RAW

}

#' @rdname parse_log_bin
#' @keywords internal
init_RAW <- function(info) {
  get_times(
    info$Start_Date,
    info$Last_Sample_Time,
    info$Sample_Rate
  ) %>%
  {data.frame(
    Timestamp = lubridate::with_tz(., tz)
  )}
}

#' @rdname parse_log_bin
#' @keywords internal
get_RAW1 <- function(info, set, log, verbose) {

  get_primary_accel_scale(info) %>%
  legacy_parse_primary_accelerometerC(
    set, log, ., info$Sample_Rate, verbose
  ) %>%
  {data.frame(
    data.table::rbindlist(.)
  )}

}

#' @rdname parse_log_bin
#' @keywords internal
get_RAW2 <- function(info, set, verbose) {

  get_primary_accel_scale(info) %>%
  dev_parse_primary_accelerometerC(
    set, ., info$Sample_Rate, verbose
  ) %>%
  {data.frame(
    data.table::rbindlist(.)
  )}

}
