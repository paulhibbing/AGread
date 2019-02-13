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
  record_headers
  
}

select_records <- function(record_headers, include) {
  
  record_types <- unlist(lapply(
    record_headers,
    function(x) {
      RECORDS$Type[match(
        x$type[1],
        as.character(RECORDS$ID)
      )]
    }
  ))
  
  keep <- record_types %in% include
  if (!any(keep)) stop(
    "gt3x file does not contain any packets specified in `include`"
  )
  
  record_headers[record_types %in% include]
  
}
