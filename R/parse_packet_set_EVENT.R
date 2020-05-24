#' @rdname parse_packet_set
#' @export
parse_packet_set.EVENT <- function(
  set, log, tz = "UTC", verbose = FALSE,
  info, ...
) {

  if (!length(set)) {

    set <- list(
      other_events = data.frame(),
      idle_sleep_events = data.frame()
    )

    if (verbose) cat(
      "\n  No EVENT packets to parse (or they\'ve",
      "been left out of `include`)"
    )

    return(structure(set, class = "EVENT"))

  }

  event_types <- lapply(
    split(set, seq(nrow(set))),
    setup_payload,
    log = log
  ) %>% sapply(
    readBin, what = "integer", size = 1
  )

  set$event_type <- ifelse(
    event_types == 8, "sleep_ON", "UNKNOWN"
  ) %>% ifelse(
    event_types == 9, "sleep_OFF", .
  ) %>% factor(
    levels = c("sleep_ON", "sleep_OFF", "UNKNOWN")
  )

  sleep_check <- grepl("^sleep", set$event_type)

  if (any(sleep_check)) {

    set <- split(set, sleep_check)

    names(set) %<>%
      gsub("^TRUE$", "idle_sleep_events", .) %>%
      gsub("^FALSE$", "other_events", .)

    set$idle_sleep_events <- format_sleep_events(
      set$idle_sleep_events, info, tz, verbose
    )

    if (!"other_events" %in% names(set)) {
      set$other_events <- data.frame()
      set <- set[c("other_events", "idle_sleep_events")]
    }

  } else {

    set <- list(
      other_events = set, idle_sleep_events = data.frame()
    )

  }

  if (verbose) packet_print("cleanup", "EVENT")

  structure(set, class = "EVENT")

}
