#' Parse the info component of a gt3x file
#'
#' @param info connection to the info.txt file
#' @param tz character. The timezone
#' @param verbose logical. Print updates to console?
#' @param ... further arguments/methods. Currently unused.
#'
#' @keywords internal
#'
parse_info_txt <- function(info, tz = "UTC", verbose, ...) {

  if (verbose) cat("\n  Parsing info.txt")

  ## Read text file and assemble data frame

    meta <-
      readLines(info) %>%
      strsplit(": ") %>%
      {stats::setNames(
        sapply(., "[[", 2),
        sapply(., "[[", 1)
      )} %>%
      stats::setNames(., gsub(" ", "_", names(.))) %>%
      t(.) %>%
      data.frame(stringsAsFactors = FALSE)

  ## Format numeric variables

    num_vars <- c(
      "Battery_Voltage", "Sample_Rate", "Board_Revision",
      "Unexpected_Resets", "Acceleration_Scale",
      "Acceleration_Min", "Acceleration_Max"
    )

    stopifnot(all(num_vars %in% names(meta)))

    for (i in num_vars) {
      meta[ ,i] <- as.numeric(
        as.character(meta[ ,i])
      )
    }

  ## Format time variables

    tick_vars <- c(
      "Start_Date", "Stop_Date",
      "Last_Sample_Time", "Download_Date"
    )

    stopifnot(all(tick_vars %in% names(meta)))

    for (j in tick_vars) {
      meta[ ,j] <- tick_to_posix(
        meta[ ,j], tz
      )
    }

    meta$Download_Date <- strftime(
      meta$Download_Date,
      "%m/%d/%Y"
    )

  ## Find lux information and finish up

    meta %>%
    {.$Serial_Number} %>%
    grepl("MOS", .) %>%
    ifelse(4, 3) %>%
    substring(meta$Serial_Number, 1, .) %>%
    sapply(list(.lux_scale, .lux_max), "[", .) %>%
    stats::setNames(c("lux_scale", "lux_max")) %>%
    t(.) %>%
    data.frame(.) %>%
    data.frame(meta, .) %T>%
    {if (verbose) cat("  ............. COMPLETE")}

}
