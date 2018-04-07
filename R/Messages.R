#' Print processing updates to console
#'
#' @param n The message number to print
#' @param is_message A logical scalar: Print as message?
#' @param file A character scalar: name of file being processed
#'
#' @keywords internal
#'
message_update <- function(n, is_message = FALSE, file, dur) {
  note <-
    switch(n,
      paste("\n\n\nProcessing:", basename(file)),
      "\n....Naming columns based on file mode",
      "\n....Using automatically-detected column names",
      "\n..Getting meta-data from header",
      "\n..Reading file",
      "\n....Attempting to determine column names from mode and column characteristics",
      "\n......No sign of confounding by date/time variables",
      "\n......Affirmative evidence of confounding by date/time variables",
      "\n......No conclusion about date/time confounding possible",
      "\n......Identified a vector magnitude variable included in the file",
      "\n......No sign of confounding by vector magnitude variable",
      "\n......Success identifying remaining columns",
      "Tests indicate that inclinometer variables have been mis-assigned. Review the file by hand, and submit a bug report.",
      "\nAll columns read as character.\nReturning prematurely as a result.\nAdjusting value of skip parameter will likely fix the issue.",
      "\n......Checks on inclinometer variables showed no issues",
      paste("\nProcessing complete. Elapsed time", dur, "minutes."),
      "\n17")
  if (is_message) {
    message(note)
  } else {
    cat(note)
  }
}
