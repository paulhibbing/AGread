# Generic function---------------------------------------------------------

  parse_packet_set <- function(
    set, log, tz = "UTC", verbose = FALSE,
    give_timestamp = TRUE, ...
  ) {

    if (verbose) packet_print("startup", class(set)[1])
    UseMethod("parse_packet_set", set)

  }

# Simple methods ----------------------------------------------------------

  #' @rdname parse_packet_set
  #' @export
  parse_packet_set.default <- function(
    set, log, tz = "UTC", verbose = FALSE,
    give_timestamp = TRUE, ...
  ) {

    warning(paste(
      "No method exists yet for parsing",
      class(set)[1], "packets -- returning NULL."
    ))

    return(NULL)

  }

  #' @rdname parse_packet_set
  #' @export
  parse_packet_set.PARAMETERS <- function(
    set, log, tz = "UTC", verbose = FALSE,
    give_timestamp = TRUE, ...
  ) {

    payload <- setup_payload(set, log) %>%
      split(cumsum(seq(.) %% 8 == 1)) %>%
      {do.call(rbind, lapply(., process_parameters, tz = tz))} %>%
      {.[.$Label != "UNUSED KEY", ]} %>%
      {stats::setNames(as.list(.$value), .$Label)}

    value <- list(
      Type = class(set)[1],
      Timestamp = lubridate::force_tz(
        set$timestamp, tz
      ),
      Payload_Size = set$payload_size,
      Payload = payload,
      Checksum = "OK"
    )

    if (!give_timestamp) value$Timestamp <- NULL

    if (verbose) packet_print("cleanup", class(set)[1])

    structure(value, class = class(set)[1])

  }
