#' @rdname parse_packet_set
#' @param payload a raw vector containing packet payload
#' @keywords internal
capsense_payload <- function(payload) {

  stopifnot(length(payload) == 6)

  signal <- readBin(
    payload[1:2], "integer", 2, 2, FALSE
  )
  reference <- readBin(
    payload[3:4], "integer", 2, 2, FALSE
  )
  state <- readBin(
    payload[5], "integer", 1, 1, FALSE
  )

  if (state == 0) {
    state <- "Not Worn"
  } else {
    state <- "Worn"
  }

  bursts <- readBin(
    payload[6], "integer", 1, 1, FALSE
  )

  data.frame(
    signal = signal,
    reference = reference,
    state = state,
    bursts = bursts,
    stringsAsFactors = FALSE
  )

}

#' @rdname parse_packet_set
#' @export
parse_packet_set.CAPSENSE <- function(
  set, log, tz = "UTC", verbose = FALSE,
  ...
) {

  results <- lapply(
    split(set, seq(nrow(set))),
    function(x) {
      setup_payload(x, log) %>%
      capsense_payload(.)
    }
  )

  if (verbose) packet_print("cleanup", class(set)[1])

  collapse_packet_set(set, results)

}

#' @rdname parse_packet_set
#' @export
parse_packet_set.METADATA <- function(
  set, log, tz = "UTC", verbose = FALSE,
  payload = NULL, ...
) {

  if (is.null(payload)) {
    payload <-
      nrow(set) %>%
      seq(.) %>%
      split(set, .) %>%
      lapply(setup_payload, log = log)
  }

  results <-
    payload %>%
    lapply(function(x) {
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
    }) %>%
    c(make.row.names = FALSE) %>%
    do.call(rbind, .)

  drop_rows <- grepl("^Parsed:", results$Meta) |
    grepl("^null$", results$Meta)

  results %<>%
    .[!drop_rows, ] %>%
    {split(., cumsum(grepl("^MetadataType", .)))} %>%
    {.[!duplicated(.)]}

  meta_names <- sapply(
    results,
    function(x) gsub("^MetadataType:", "", x[1]),
    USE.NAMES = FALSE
  )

  results %<>%
    stats::setNames(meta_names) %>%
    lapply(function(x) x[-1])

  if (verbose) packet_print("cleanup", class(set)[1])

  structure(results, class = class(set)[1])

}
