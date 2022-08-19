# Input checking ----------------------------------------------------------

validate_include <- function(
  include,
  verbose = FALSE
) {

  stopifnot(all(include %in% .packets))
  include <- match.arg(include, c(.packets, "Error"), TRUE)

  if (verbose) {
    CHOICES <- split(include, cumsum(seq(include)%%4 == 1))
    CHOICES <- lapply(CHOICES, function(x) paste(x, collapse = ", "))
    cat("\n\n  Will parse the following packet types, if available:\n")
    lapply(CHOICES, function(x) cat("   ", x, "\n"))
  }

  include

}


validate_parser <- function(parser) {

  if (identical(
    parser, c("legacy", "dev")
  )) return("legacy")

  parser %T>%
  {stopifnot(
    . %in% c("legacy", "dev"),
    length(.) == 1
  )}

}


# Record header retrieval & formatting ------------------------------------

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


select_records <- function(record_headers, include) {

  keep <- names(record_headers) %in% include
  if (!any(keep)) stop(
    "gt3x file does not contain any packets specified in `include`"
  )

  record_headers[names(record_headers) %in% include]

}


# Packet parsing ----------------------------------------------------------

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


get_temp_offset <- function(parameters) {

  temp_offset <- 21

  if (is.null(parameters)) {
    warning(
      "No `parameters` argument passed to ",
      "get_temp_offset. 21 degress will be\n  assumed as the offset.",
      " Make sure that\'s correct by making a read_gt3x call\n  ",
      "that has (minimally) the following:\n  `include =",
      " c(\"SENSOR_SCHEMA\", \"SENSOR_DATA\", \"PARAMETERS\")`",
      call. = FALSE
    )
    return(temp_offset)
  }

  if (!"IMU_TEMP_OFFSET" %in% names(parameters)) {
      warning(
        "PARAMETERS object has no `IMU_TEMP_OFFSET` entry.",
        " Defaulting to 21 degrees.", call. = FALSE
      )
      return(temp_offset)
    }

  parameters$IMU_TEMP_OFFSET %>%
  as.character(.) %>%
  as.numeric(.)

}
