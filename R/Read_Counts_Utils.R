#' Capitalize a string vector
#'
#' First character will be capitalized, and all others remain unchanged
#'
#' @param string the vector to capitalize. Scalars accepted.
#' @keywords internal
capitalize <- function(string) {
  sapply(string, function(x) paste(
    toupper(substring(x, 1, 1)),
    substring(x, 2),
    collapse = "",
    sep = ""
  ), USE.NAMES = FALSE)
}

#' Extract meta-data from file header
#'
#' @inheritParams read_AG_counts
#'
#' @examples
#' counts_file <- system.file(
#'   "extdata", "example1sec.csv", package = "AGread"
#' )
#' AGread::AG_meta(counts_file)
#'
#' @export
AG_meta <- function(
  file, verbose = FALSE,
  header_timestamp_format = "%m/%d/%Y %H:%M:%S"
) {

  if (verbose) message_update(4)

  values <-
    vector(mode = "list", 4) %>%
    stats::setNames(
      c("start_time", "start_date", "epoch", "mode")
    )

  start_line <- 0

  while (value_check(values)) {
    x <- readr::read_lines(file, start_line, TRUE, 1)
    if (grepl("start time", x, TRUE)) {
      values$start_time <-
        meta_format(x) %T>%
        {stopifnot(grepl(
          "^[0-9]{1,2}:[0-9]{2}:[0-9]{2}$", .
        ))}
    }
    if (grepl("start date", x, TRUE)) {
      values$start_date <-
        meta_format(x) %T>%
        {stopifnot(
          grepl("[0-9]{1,2}[/-][0-9]{1,2}[/-][0-9]{2,4}$", .) |
          grepl("[0-9]{2,4}[/-][0-9]{1,2}[/-][0-9]{1,2}$", .)
        )}
    }
    if (grepl("epoch", x, TRUE)) {
      values$epoch <-
        meta_format(x) %T>%
        {stopifnot(grepl(
          "^[0-9]{1,2}:[0-9]{2}:[0-9]{2}$", .
        ))}
    }
    if (grepl("mode", x, TRUE)) {
      values$mode <-
        meta_format(x) %T>%
        {stopifnot(grepl(
          "^[0-9]*$", .
        ))}
    }
    if (start_line > 50) {
      break
    } else {
      start_line %<>% {. + 1}
    }
  }

  if (value_check(values)) {
    stop(
      "Couldn\'t identify start time, start date, epoch,",
      " and mode within first 50 lines of ", basename(file),
      call. = FALSE
    )
  }

  start <-
    paste(values$start_date, values$start_time) %>%
    as.POSIXct("UTC", header_timestamp_format)

  if (is.na(start)) {
    stop(
      "\nFailed to parse start date/time from header of `",
      basename(file), "`\nYou may need to pass a different",
      " value for header_timestamp_format,\ne.g.",
      " `header_timestamp_format = \"%Y-%m-%d %H:%M:%S\"`",
      call. = FALSE
    )
  }

  epoch <-
    values$epoch %>%
    strsplit(":") %>%
    unlist(.) %>%
    as.numeric(.) %>%
    {. * c(3600, 60, 1)} %>%
    sum(.)

  mode <- as.numeric(values$mode)

  list(start = start, epoch = epoch, mode = mode)

}

#' @rdname AG_meta
#' @param values list of meta values to check for completeness
#' @keywords internal
value_check <- function(values) {
  sapply(values, is.null) %>%
  any(.)
}

#' @rdname AG_meta
#' @param x string to format (one line of input from the data file)
#' @keywords internal
meta_format <- function(x) {
  gsub(",*$", "", x) %>%
  strsplit(" ") %>%
  sapply(function(y) y[length(y)], USE.NAMES = FALSE)
}

#' Map variable inclusion to columns in csv file
#'
#' @param variable A variable included in the output
#'
#' @keywords internal
AG_col_names <- function(variable) {
  switch(variable,
         "axis1" = "Axis1",
         "axis2" = "Axis2",
         "axis3" = "Axis3",
         "steps" = "Steps",
         "heart_rate" = "Heart.Rate",
         "lux" = "Lux",
         "incline" = c(
           "Inclinometer.Off",
           "Inclinometer.Standing",
           "Inclinometer.Sitting",
           "Inclinometer.Lying"))
}

#' Test whether inclinometer variables have been correctly assigned
#'
#' @param AG the monitor data to check
#'
#' @keywords internal
#'
check_inc <- function(AG, verbose = FALSE) {
  inc_names <-
    which(grepl("inclinometer", names(AG), ignore.case = TRUE))
  test1 <- length(inc_names) == 4
  test2 <- all(sapply(AG[ ,inc_names], function(y) all(y %in% 0:1)))
  if (!(test1 & test2)) {
    message_update(13, TRUE)
    return(AG)
  }

  if (verbose) message_update(15)
  return(AG)
}

#' Add time variable to processed data
#'
#' @inheritParams check_inc
#' @param meta A list given by \code{\link{AG_meta}}
#'
#' @keywords internal
#'
AG_time <- function(AG, meta) {
  AG$Timestamp <-
    meta$start +
    (meta$epoch * (seq(nrow(AG)) - 1))

  AG <- AG[ ,c("Timestamp", setdiff(names(AG), "Timestamp"))]
  return(AG)
}
