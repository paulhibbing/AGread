# Documentation -----------------------------------------------------------

#' @title Deprecated functions in package \pkg{AGread}.
#' @description These are functions that are deprecated because they have been
#'   moved elsewhere, primarily to the PAutilities package. Use
#'   \code{help("AGread-deprecated")} to access documentation for deprecated
#'   functions.
#' @name AGread-deprecated
#' @keywords internal
NULL

#' Provide the run time of processing
#'
#' @param timer a proc_time object giving the initial time
#'
#' @examples
#' timer <- proc.time()
#' Sys.sleep(2.2)
#' \donttest{
#'   # Gives warning about deprecation
#'   # get_duration(timer)
#'
#'   # Instead use
#'   PAutilities::get_duration(timer)
#' }
#'
#'
#' @name get_duration-deprecated
#' @usage get_duration(timer)
#' @seealso \code{\link{AGread-deprecated}}
#' @keywords internal
NULL

# Functions ---------------------------------------------------------------

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
