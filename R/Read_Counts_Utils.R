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
AG_meta <- function(file, verbose = FALSE, ...) {
  if (verbose) message_update(4)
  meta <-
    utils::read.csv(file,
                    stringsAsFactors = FALSE,
                    ...)


  ##Get and check indices
  start_time <-
    which(grepl("start time", meta[ ,1], ignore.case = TRUE))
  start_date <-
    which(grepl("start date", meta[ ,1], ignore.case = TRUE))
  epoch <-
    which(grepl("epoch", meta[ ,1], ignore.case = TRUE))
  mode <-
    which(grepl("mode", meta[ ,1], ignore.case = TRUE))

  stopifnot(
    all(
      sapply(c(start_time, start_date, epoch, mode),
             length) == 1)
  )

  ##Format values
  values <-
    strsplit(meta[c(start_time, start_date, epoch, mode), ], " ")
  values <-
    stats::setNames(lapply(values, function(x) x[length(x)]),
                    c("start_time", "start_date", "epoch", "mode"))

  start <- paste(values$start_date, values$start_time)
  start <- as.POSIXct(start, "UTC", "%m/%d/%Y %H:%M:%S")

  epoch <- as.numeric(unlist(strsplit(values$epoch, ":")))
  epoch <- sum((epoch * c(3600, 60, 1)))

  mode <- as.numeric(values$mode)
  list(start = start, epoch = epoch, mode = mode)
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
