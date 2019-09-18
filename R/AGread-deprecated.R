#' @title Deprecated functions in package \pkg{AGread}.
#' @description These are functions that are deprecated because they have been
#'   moved elsewhere, primarily to the PAutilities package. Use
#'   \code{help("AGread-deprecated")} to access documentation for deprecated
#'   functions.
#' @name AGread-deprecated
#' @keywords internal
NULL

#' @rdname AGread-deprecated
#' @section \code{get_duration}:
#' For \code{get_duration}, use \code{PAutilities::get_duration}
#'
#' @export
get_duration <- function(timer) {

  .Deprecated("PAutilities::get_duration", "PAutilities")
  format(
    (proc.time() - timer)[3] / 60,
    digits = 1,
    nsmall = 1
  )
}
