#' Collapse a set of processed gt3x packets
#'
#' @inheritParams parse_packet_set
#' @param results the processed packet information
#'
#' @keywords internal
collapse_packet_set <- function(set, results, ...) {

  UseMethod("collapse_packet_set", set)

}

#' @rdname collapse_packet_set
#' @export
collapse_packet_set.default <- function(set, results, ...) {

  results <- do.call(rbind, results)
  new_names <- append(names(results), "Timestamp", 0)

  results$Timestamp <- set$timestamp
  results <- results[ ,new_names]

  row.names(results) <- NULL
  structure(results, class = class(set))

}
