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

#' Read data table files containing count values
#'
#' @param file A character scalar giving path to an automatically-generated csv
#'   file with count values
#' @param verbose A logical scalar: Print processing updates?
#' @param skip Header length: Number of rows to skip when reading the file
#' @param nrows Header length: Number of rows to read when retrieving meta-data
#' @param header A logical scalar: Are variable names contained in first row of
#'   file?
#' @param ... Further arguments passed to \code{read.csv} and \code{fread}
#'
#' @return A data frame reflecting the data contained in the csv file
#' @export
#'
#' @examples
#' read_AG_counts(
#'   system.file("example1sec.csv", package = "AGread"),
#'   skip = 11
#' )
#'
read_AG_counts <- function(file, verbose = FALSE, skip = 10,
    nrows = 10, header = FALSE, ...) {

  timer <- proc.time()

  # Get metadata
    if (verbose) message_update(1, file = file)
      meta <-
        AG_meta(
          file,
          verbose = verbose,
          nrows = nrows,
          header = header,
          ...
        )
    file_mode <-
      modes[modes$mode == meta$mode, ]
    stopifnot(nrow(file_mode) == 1)

  # Pick out expected variable names
    matches <-
      sapply(names(file_mode), function(x) file_mode[ ,x] == "X")
    mode_vars <-
      names(matches)[matches]
    variables <-
      do.call(c,
        sapply(
          mode_vars,
          AG_col_names,
          simplify = FALSE,
          USE.NAMES = FALSE
        ))

  # Read the file
    if (verbose) message_update(5)
    AG <-
      data.frame(data.table::fread(file,
        stringsAsFactors = FALSE,
        skip = skip,
        header = header,
        ...))

  # Deal with mis-reading case
    if (all(sapply(AG, class) %in% c("character", "factor"))) {
      message_update(14, TRUE)
      return(AG)
    }

  # Deal with the automatic-naming case for dummy-coded inclinometer
    if (length(variables) == ncol(AG)) {
      if (verbose) {
        message_update(2)
      }
      names(AG) <- variables
      AG <- AG_time(AG, meta)
      if (verbose) message_update(16, dur = get_duration(timer))
      return(AG)
    }

  # Deal with the automatic-naming case for discrete (0-3) inclinometer
    if (length(mode_vars) == ncol(AG)) {
      if (verbose) {
        message_update(28)
      }
      names(AG) <- mode_vars
      AG <- AG_time(AG, meta)
      if (verbose) message_update(16, dur = get_duration(timer))
      return(AG)
    }

  # Deal with cases not caught in previous cases
  if (!"V1" %in% names(AG)) {
    if (verbose) {
      message_update(3)
    }
    AG <- AG_time(AG, meta)
    if (verbose) message_update(16, dur = get_duration(timer))
    return(AG)
  } else {
    if (verbose) message_update(6)
    AG <- test_times(AG, verbose = verbose)

    if (all(c("Axis1", "Axis2", "Axis3") %in% variables)) {
      AG <- test_VM(AG, verbose = verbose)
    }

    variables <- variables[!variables %in% names(AG)]
    missing_names <- names(AG)[grepl("^V[0-9]*$", names(AG))]

    if (length(variables) == length(missing_names)) {
      if (verbose) message_update(12)
      names(AG)[grepl("^V[0-9]*$", names(AG))] <- variables
    }

    if (any(grepl("inclinometer", names(AG), ignore.case = TRUE))) {
      AG <- check_inc(AG, verbose = verbose)
    }

    AG <- AG_time(AG, meta)
    if (verbose) message_update(16, dur = get_duration(timer))
    return(AG)
  }
}

#' Test whether first columns contain date/time information
#'
#' @param AG A data frame of partially-processed data
#' @inheritParams read_AG_counts
#'
#' @keywords internal
#'
test_times <- function(AG, verbose = FALSE) {
  classes <- sapply(AG, class)
  test <-
    any(classes %in% c("character", "factor"))

  if ((!test)) {
    if (verbose) message_update(7)
    return(AG)
  }

  test2 <- all(classes[1:2] %in% c("character", "factor"))
  test3 <- grepl("/", AG[1,1]) & grepl(":", AG[1,2])
  if (test2 & test3) {
    if (verbose) message_update(8)
    names(AG)[1:2] <- c("Date", "Time")
    return(AG)
  }

  if (verbose) message_update(9)
  return(AG)
}

#' Test whether a vector magnitude column is present
#'
#' @inheritParams test_times
#'
#' @keywords internal
#'
test_VM <- function(AG, verbose = FALSE) {
  classes <- sapply(AG, class)
  numerics <- AG[ ,which(!classes %in% c("character", "factor"))]
  numerics <- numerics[ ,!sapply(numerics, function(x) all(x %in% 0:1))]
  test_VM <- apply(numerics[ ,1:3], 1, function(x) round(sqrt(sum(x^2)), 2))
  test_VM <- sapply(which(!classes %in% c("character", "factor")),
    function(y)
      all.equal(test_VM, AG[, y]) == TRUE)
  test_VM <-
    which(!classes %in% c("character", "factor"))[test_VM]

  if (length(test_VM) == 1) {
    if (verbose) message_update(10)
    names(AG)[test_VM] <- "Vector.Magnitude"

    names(numerics)[1:3] <- c("Axis1", "Axis2", "Axis3")
    a1_index <-
      sapply(AG, function(y) unique(all.equal(y, numerics$Axis1) == TRUE))
    a2_index <-
      sapply(AG, function(y) unique(all.equal(y, numerics$Axis2) == TRUE))
    a3_index <-
      sapply(AG, function(y) unique(all.equal(y, numerics$Axis3) == TRUE))

    names(AG)[a1_index] <- "Axis1"
    names(AG)[a2_index] <- "Axis2"
    names(AG)[a3_index] <- "Axis3"

    return(AG)
  }

  if (verbose) message_update(11)
  return(AG)
}

#' Add time variable to processed data
#'
#' @inheritParams test_times
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

#' Test whether inclinometer variables have been correctly assigned
#'
#' @inheritParams test_times
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

#' Provide the run time of processing
#'
#' @param timer The initial time
#'
#' @examples
#' timer <- proc.time()
#' Sys.sleep(2.2)
#' AGread:::message_update(16, dur = get_duration(timer))
#'
#' @export
#'
get_duration <- function(timer) {
  format((proc.time() - timer)[3] / 60, digits = 1, nsmall = 1)
}
