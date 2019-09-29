#' @rdname parse_packet_set
#' @param info the result of \code{\link{parse_info_txt}}
#' @param events the result of parsing EVENTS packets
#' @export
parse_packet_set.ACTIVITY2 <- function(
  set, log, tz = "UTC", verbose = FALSE,
  info, events, ...
) {

  init <- get_times(
    info$Start_Date,
    info$Last_Sample_Time,
    info$Sample_Rate
  ) %>% {data.frame(
   Timestamp = lubridate::with_tz(
     ., tz
   )
  )}

  RAW <- get_primary_accel_scale(
    info
  ) %>% parse_primary_accelerometerC(
    set, log, ., info$Sample_Rate, verbose
  ) %>% {data.frame(
    data.table::rbindlist(.)
  )}

  RAW$Timestamp <- lubridate::with_tz(
    RAW$Timestamp, tz
  )

  RAW <- idle_sleep_impute(
    RAW, events, info, tz, verbose
  ) %>% merge(
    init, ., "Timestamp", all.x = TRUE
  ) %>% impute_primary(
    verbose
  ) %>% {structure(
    ., class = append(class(.), "RAW", 0)
  )}

  if (verbose) packet_print("cleanup", class(set)[1])

  RAW

}
