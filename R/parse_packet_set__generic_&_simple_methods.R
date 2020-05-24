# Generic function---------------------------------------------------------

#' Parse all packets of a given type
#'
#' @param set the set of record headers corresponding to each packet
#' @param log the raw data from \code{log.bin}
#' @inheritParams read_gt3x
#' @param ... further arguments passed to methods
#'
#' @keywords internal
parse_packet_set <- function(
  set, log, tz = "UTC", verbose = FALSE,
  ...
) {

  if (verbose) packet_print("startup", class(set)[1])
  UseMethod("parse_packet_set", set)

}

# Simple methods ----------------------------------------------------------

#' @rdname parse_packet_set
#' @export
parse_packet_set.default <- function(
  set, log, tz = "UTC", verbose = FALSE,
  ...
) {

  warning(
    "No method exists yet for parsing ",
    class(set)[1], " packets -- returning NULL."
  )

  return(NULL)

}

#' @rdname parse_packet_set
#' @export
parse_packet_set.PARAMETERS <- function(
  set, log, tz = "UTC", verbose = FALSE,
  ...
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

  if (verbose) packet_print("cleanup", class(set)[1])

  structure(value, class = class(set)[1])

}

#' @rdname parse_packet_set
#' @export
parse_packet_set.BATTERY <- function(
  set, log, tz = "UTC", verbose = FALSE,
  ...
) {

  results <- lapply(
    split(set, seq(nrow(set))),
    function(x) data.frame(
      battery_voltage = readBin(
        setup_payload(x, log), "integer",
        2, 2, FALSE
      ) / 1000
    )
  )

  if (verbose) packet_print("cleanup", class(set)[1])

  collapse_packet_set(set, results)

}
