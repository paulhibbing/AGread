#' Print processing updates to console
#'
#' @inheritParams read_AG_IMU
#' @param message_number The message number to print
#' @param is_message A logical scalar: Print as message?
#' @param file A character scalar: name of file being processed
#' @param vm_variables variables being used to calculate vector magnitude
#' @param n Number of replications to be performed for magnetometer direction classification
#'
#' @keywords internal
#'
message_update <- function(message_number, is_message = FALSE, file, dur, vm_variables, n, filter_hz) {
  note <-
    switch(message_number,
      paste("\n\n\nReading:", basename(file)),
      "\n....Naming columns based on file mode",
      "\n....Using automatically-detected column names",
      "\n..Getting meta-data from header",
      "\n..Reading file",
      #^^5

      "\n....Attempting to determine column names from mode and column characteristics",
      "\n......No sign of confounding by date/time columns",
      "\n......Affirmative evidence that date and time are first two columns",
      "\n......No conclusion about date/time confounding possible",
      "\n......Identified a vector magnitude variable included in the file",
      #^^10

      "\n......No sign of confounding by vector magnitude variable",
      "\n......Success identifying remaining columns",
      "Tests indicate that inclinometer variables have been mis-assigned. Review the file by hand, and submit a bug report.",
      "\nAll columns read as character.\nReturning prematurely as a result.\nAdjusting value of skip parameter will likely fix the issue.",
      "\n......Checks on inclinometer variables showed no issues",
      #^^15

      paste("\nReading complete. Elapsed time", dur, "minutes."),
      "Primary accelerometer file is formatted unexpectedly. Processing with read.csv() -- be prepared to wait.",
      "Error in file formatting. Returning NULL.",
      paste("\n\n-- Low-pass filtering gyroscope at", filter_hz, "Hz..."),
      " Done.\n",
      #^^20

      "\n-- Calculating Vector Magnitudes...",
      paste(
        "\n     Getting VM for variables searched on the following criteri(a/on):",
        vm_variables,
        "\n"
      ),
      "\n     Vector magnitude calculation complete.\n",
      "Number of rows not divisible by samp_rate*output_window: Data will be truncated.",
      "\n-- Collapsing data. This could take awhile...",
      #^^25

      "Length of X and Y differ. Returning NULL.",
      paste("Determining direction from mean values of x and y, replicating", n, "times."),
      "\n28")
  if (is_message) {
    message(note)
  } else {
    cat(note)
  }
}
