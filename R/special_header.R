#' Individually process a specific record_header
#'
#' @param record_headers list of record header information for the various
#'   packet types
#' @param type character. The type code for the target record header
#' @inheritParams process_record_set
#'
#' @keywords internal
special_header <- function(
  record_headers, type, log, tz, info, give_timestamp,
  verbose = FALSE, do_post_process = FALSE
) {

  index <- which(record_headers$type == type)

  if (!length(index)) {

    return(NULL)

  }

  list(
    result = process_record_set(
      record_headers[index, ],
      log, tz, info, give_timestamp,
      verbose = verbose, do_post_process = FALSE
    ),
    index = index
  )

}
