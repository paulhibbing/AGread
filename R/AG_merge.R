#' Merge primary accelerometer data with IMU data
#'
#' @param AG object obtained from \code{\link{read_gt3x}}
#' @param primary_args additional arguments for \code{\link{collapse_gt3x.RAW}}
#' @param IMU_args additional arguments for \code{\link{collapse_gt3x.IMU}}
#' @param common_args additional arguments to pass to both
#'   \code{\link{collapse_gt3x.RAW}} and \code{\link{collapse_gt3x.IMU}}
#' @param verbose logical. Print information to console?
#' @param ... additional arguments passed to \code{merge}
#'
#' @return a data frame of merged data
#' @export
#'
#' @examples
#' \donttest{
#' file_3x <- system.file(
#'   "extdata", "example.gt3x", package = "AGread"
#' )
#' AG_3x <- read_gt3x(file_3x)
#' head(AG_merge(AG_3x))
#' }
AG_merge <- function(
  AG, primary_args = list(), IMU_args = list(),
  common_args = list(), verbose = FALSE, ...
) {

  all_args <- c(names(primary_args), names(IMU_args))
  if ("method" %in% all_args) stop(paste(
    "You may not pass `method` as an",
    " argument in `primary_args` or `IMU_args`.",
    "\n  `AG_merge` uses `method = \"expanded\"` automatically",
    "and exclusively."
  ))

  stopifnot(all(c("RAW", "IMU") %in% names(AG)))

  if (verbose) timer <- PAutilities::manage_procedure(
    "Start", "\nPerforming a gt3x primary/IMU merge"
  )

  primary <- do.call(
    collapse_gt3x, c(
      list(AG = AG$RAW),
      primary_args,
      common_args,
      list(method = "expanded")
    )
  )

  IMU <- do.call(
    collapse_gt3x, c(
      list(AG = AG$IMU),
      IMU_args,
      common_args,
      list(method = "expanded")
    )
  )

  result <- merge(
    primary, IMU, "Timestamp", ...
  )

  names(result) <- gsub(
    "\\.x$", "", names(result)
  ) %>% gsub(
    "\\.y$", "_IMU", .
  )

  if (verbose) PAutilities::manage_procedure(
    "End", "\n...Merge complete.", "Elapsed time",
    PAutilities::get_duration(timer), "minutes."
  )

  result

}
