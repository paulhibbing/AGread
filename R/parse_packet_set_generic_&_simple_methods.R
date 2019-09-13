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

#' @rdname parse_packet_set
#' @export
parse_packet_set.METADATA <- function(
  set, log, tz = "UTC", verbose = FALSE,
  give_timestamp = TRUE, ...
) {

  results <- lapply(
    split(set, seq(nrow(set))),
    setup_payload,
    log = log
  ) %>% lapply(
    function(x) {
      gsub("[{}]", "", rawToChar(x)) %>%
      gsub(pattern = "[\"]", replacement = "") %>%
      gsub(pattern = "\\\\", replacement = "") %>%
      gsub(pattern = "JSON:", replacement = "") %>%
      strsplit(",") %>%
      {do.call(data.frame,
        c(., stringsAsFactors = FALSE,
          row.names = NULL)
      )} %>%
      stats::setNames("Meta")
    }
  )

  results <- do.call(
    rbind, c(results, make.row.names = FALSE)
  )

  drop_rows <- grepl("^Parsed:", results$Meta) |
    grepl("^null$", results$Meta)

  results <- results[!drop_rows, ]

  results <- split(
    results, cumsum(grepl("^MetadataType", results))
  )

  results <- results[!duplicated(results)]
  meta_names <- sapply(
    results,
    function(x) gsub("^MetadataType:", "", x[1]),
    USE.NAMES = FALSE
  )

  results <- stats::setNames(results, meta_names)
  results <- lapply(results, function(x) x[-1])

  structure(results, class = class(set)[1])

}
