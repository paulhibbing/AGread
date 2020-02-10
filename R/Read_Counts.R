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
#' AG_counts <- read_AG_counts(
#'   system.file(
#'     "extdata",
#'     "example1sec.csv",
#'     package = "AGread"
#'   ),
#'   skip = 11
#' )
#' head(AG_counts)
#'
read_AG_counts <- function(file, verbose = FALSE, skip = 10,
    nrows = 10, header = FALSE, ...) {

  timer <- PAutilities::manage_procedure(
    "Start", "\nReading", basename(file), verbose = verbose
  )

  # Get metadata

    if (verbose) message_update(1, file = file)

    meta <- AG_meta(
      file,
      verbose = verbose,
      nrows = nrows,
      header = header,
      ...
    )

    file_mode <- modes[modes$mode == meta$mode, ]
    stopifnot(nrow(file_mode) == 1)

  # Pick out expected variable names

    mode_vars <-
      names(file_mode) %>%
      sapply(function(x) file_mode[ ,x] == "X") %>%
      {names(.)[.]}

    variables <-
      sapply(
        mode_vars, AG_col_names,
        simplify = FALSE, USE.NAMES = FALSE
      ) %>%
      do.call(c, .)

  # Read the file

    if (verbose) message_update(5)

    AG <-
      data.table::fread(
        file,
        stringsAsFactors = FALSE,
        skip = skip,
        header = header,
        ...
      ) %>%
      data.frame(stringsAsFactors = FALSE, row.names = NULL)

  # Deal with mis-reading case

    if (all(sapply(AG, class) %in% c("character", "factor"))) {
      message_update(14, TRUE)
      return(AG)
    }

  # Check for date/time variables

    time_vars <- sapply(AG[ ,1:2], function(x) {
      is.character(x) & any(grepl("[/:]", x))
    })

    if (all(time_vars)) {

      if (verbose) message_update(8)
      mode_vars <- append(
        mode_vars, c("Date", "Time"), 0
      )
      variables <- append(
        variables, c("Date", "Time"), 0
      )

      first_col <- 3

    } else {

      first_col <- 1

    }

  # Check for vector magnitude

    if (verbose) cat("\n......Checking for vector magnitude")

    vm_test <-
      get_VM_C(AG[ ,first_col], AG[ ,first_col+1], AG[ ,first_col+2]) %>%
      round(2) %>%
      all.equal(AG[ ,ncol(AG)], tolerance = 0.015) %>%
      isTRUE(.)

    if (vm_test & (!"Vector.Magnitude" %in% variables)) {
      mode_vars <- append(mode_vars, "Vector.Magnitude")
      variables <- append(variables, "Vector.Magnitude")
    }

  # Deal with the automatic-naming case for dummy-coded inclinometer

    if (length(variables) == ncol(AG)) {

      if (verbose) {
        message_update(2)
      }

      names(AG) <- variables
      AG <- AG_time(AG, meta)

      if (verbose) message_update(
        16, dur = PAutilities::get_duration(timer)
      )

      return(AG)

    }

  # Deal with the automatic-naming case for discrete (0-3) inclinometer

    if (length(mode_vars) == ncol(AG)) {

      if (verbose)  message_update(28)

      names(AG) <- capitalize(mode_vars)
      AG <- AG_time(AG, meta)
      if (verbose) message_update(16, dur = PAutilities::get_duration(timer))
      return(AG)

    }

  # Deal with cases not caught in previous cases

    if (!"V1" %in% names(AG)) {

      if (verbose) message_update(3)

      AG <- AG_time(AG, meta)
      if (verbose) message_update(
        16, dur = PAutilities::get_duration(timer)
      )
      return(AG)

    } else {

      stop(
        "File reading failed for ",
        basename(file), ".",
        " \n  Consider submitting a bug report",
        " (see `packageDescription(\"AGread\")`).",
        call. = FALSE
      )

    }

}
