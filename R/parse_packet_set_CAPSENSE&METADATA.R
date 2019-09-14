#' @rdname parse_packet_set
#' @export
parse_packet_set.CAPSENSE <- function(
  set, log, tz = "UTC", verbose = FALSE,
  ...
) {

  results <- lapply(
    split(set, seq(nrow(set))),
    function(x) {
      payload <- setup_payload(x, log)
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
  )

  if (verbose) packet_print("cleanup", class(set)[1])

  collapse_packet_set(set, results)

}

#' @rdname parse_packet_set
#' @export
parse_packet_set.METADATA <- function(
  set, log, tz = "UTC", verbose = FALSE,
  ...
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

  if (verbose) packet_print("cleanup", class(set)[1])

  structure(results, class = class(set)[1])

}
