#' @rdname parse_packet_set
#' @param info the result of \code{\link{parse_info_txt}}
#' @param events the result of parsing EVENTS packets
#'   (internal use)
#' @export
parse_packet_set.ACTIVITY2 <- function(
  set, log, tz = "UTC", verbose = FALSE,
  info, events, ...
) {

  init <-
    info %$%
    get_times(Start_Date, Last_Sample_Time, Sample_Rate) %>%
    {data.frame(
      Timestamp = lubridate::with_tz(., tz)
    )}

  RAW <-
    get_primary_accel_scale(info) %>%
    legacy_parse_primary_accelerometerC(
      set, log, ., info$Sample_Rate, verbose
    ) %>%
    data.table::rbindlist(.) %>%
    data.frame(.)

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
init_RAW <- function(info, tz) {

}
