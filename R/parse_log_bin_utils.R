# Input checking ----------------------------------------------------------

#' Validate the values provided for the \code{input} argument in
#' \code{\link{read_gt3x}}
#'
#' @inheritParams read_gt3x
#' @param choices character. The packet types to choose from.
#'
#' @keywords internal
validate_include <- function(
  include,
  verbose = FALSE,
  choices = gt3x_packets()
) {

  stopifnot(all(include %in% choices))
  include <- match.arg(include, c(choices, "Error"), TRUE)

  if (verbose) {
    CHOICES <- split(include, cumsum(seq(include)%%4 == 1))
    CHOICES <- lapply(CHOICES, function(x) paste(x, collapse = ", "))
    cat("\n\n  Will parse the following packet types, if available:\n")
    lapply(CHOICES, function(x) cat("   ", x, "\n"))
  }

  include

}

# Record header retrieval & formatting ------------------------------------

#' Retrieve record headers from .gt3x binary data
#'
#' @param log raw. The data from log.bin
#' @inheritParams parse_log_bin
#'
#' @keywords internal
#'
get_headers <- function(log, tz = "UTC", verbose = FALSE) {

  if (verbose) cat("\n")
  record_headers <- get_headersC(log, verbose)
  record_headers$index <- record_headers$index + 1

  record_headers$type  <- as.character(record_headers$type)
  record_headers$timestamp <- anytime::anytime(
    record_headers$timestamp, tz
  )

  stopifnot(
    all(
      sum(record_headers$type == "21") <= 1,
      sum(record_headers$type == "24") <= 1
    )
  )

  if (verbose) cat(
    "\r  Getting record headers",
    " ............... COMPLETE"
  )

  return(record_headers)

}

#' Sort chronologically-ordered record headers by type, for parsing one type at
#' a time
#'
#' @param record_headers the record headers to sort
#'
#' @keywords internal
#'
sort_records <- function(record_headers) {

  record_headers <- sapply(
    RECORDS$ID,
    function(x) {
      indices <- which(
        record_headers$type == as.character(x)
      )
      if (!length(indices)) return(NULL)
      record_headers[indices, ]
    },
    simplify = FALSE
  )

  record_headers[sapply(record_headers, is.null)] <- NULL

  record_types <- sapply(
    record_headers, function(x) x$type[1]
  )
  record_types <- RECORDS$Type[
    match(record_types, RECORDS$ID)
    ]

  record_headers <- mapply(
    function(x,y) structure(
      x, class = append(class(x), y, 0)
    ),
    x = record_headers, y = record_types,
    SIMPLIFY = FALSE
  )

  stats::setNames(record_headers, record_types)

}

#' Exclude record headers of types that are not listed in the \code{include}
#' argument of \code{\link{read_gt3x}}
#'
#' @param record_headers the record headers
#' @param include the packet types to include in output of
#'   \code{\link{read_gt3x}}
#'
#' @keywords internal
#'
select_records <- function(record_headers, include) {

  keep <- names(record_headers) %in% include
  if (!any(keep)) stop(
    "gt3x file does not contain any packets specified in `include`"
  )

  record_headers[names(record_headers) %in% include]

}

# Packet parsing ----------------------------------------------------------

#' Print packet-parsing information to console
#'
#' If \code{verbose} has been set to \code{TRUE} in the parent function, this
#' function will be invoked to construct the message and print it.
#'
#' @param type character. The message type
#' @param label character. The packet type
#' @param i numeric. Proportion to print as percentage for progress updates
#'
#' @keywords internal
packet_print <- function(
  type = c("startup", "progress", "cleanup"), label, i
) {

  switch(

    match.arg(type),

    "startup" = cat(
      "\n  Parsing", label, "packet(s)"
    ),

    "progress" =   cat(
      "\r  Parsing", label, "packet(s)",
      "  .............",
      paste(
        c(round(i * 100, 0), "%"),
        collapse = ""
      )
    ),

    "cleanup" = cat(
      "\r  Parsing", label, "packet(s)",
      "  ............. COMPLETE               ",
      "      "
    )

  )

}

#' Retrieve payload data for a single packet
#'
#' This function includes a call to \code{\link{checksumC}} and will break if
#' the result is not as expected
#'
#' @param record_header data frame containing information about the packet
#'   indices etc.
#' @param log raw. The data from log.bin
#'
#' @keywords internal
setup_payload <- function(record_header, log) {

  log_indices <- seq(
    record_header$index,
    record_header$index + 8 + record_header$payload_size
  )

  record <- log[log_indices]

  payload <- record[9:(length(record) - 1)]
  stopifnot(length(payload) == record_header$payload_size)

  checksumC(
    log, log_indices[1] - 1, log_indices[length(log_indices)] - 1
  )

  payload

}
