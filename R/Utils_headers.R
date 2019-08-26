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
