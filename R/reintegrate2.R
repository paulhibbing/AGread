#' A fresh approach to reintegrating ActiGraph data
#'
#' This is a \code{tidy} way of reintegrating, which is both more
#' straightforward and more concise. Runtime implications are uncertain, and
#' some functionality (notably the \code{direction} argument) has been
#' abandoned. Only forward reintegration is supported here (consistent with
#' ActiLife precedent).
#'
#' @inheritParams reintegrate
#' @param target_sec the desired epoch length of the output. Starting epoch
#'   length will be determined automatically
#'
#' @export
#'
#' @examples
#' test_file <- system.file(
#'   "extdata", "example1sec.csv", package = "AGread"
#' )
#'
#' ag <- read_AG_counts(test_file, skip = 11)
#'
#' # Old Method:
#'
#'   old_result <- reintegrate(
#'     ag = ag,
#'     to = 60,
#'     time_var = "Timestamp",
#'     direction = c("forwards")
#'   )
#'
#' # New Method:
#'
#'   new_result <- reintegrate2(ag, 60)
reintegrate2 <- function(ag, target_sec, time_var = "Timestamp") {

  e <- get_epoch(ag, time_var)

  if (e == target_sec) return(ag)

  if (e > target_sec) stop(
    "Cannot reintegrate to a shorter epoch length (`target_sec` == ", target_sec,
    ", but `get_epoch(AG)` == ", e, ")", call. = FALSE
  )

  ag %>%
  dplyr::group_by(
    !!as.name(time_var) := lubridate::floor_date(
      !!as.name(time_var), paste(target_sec, "sec"))
  ) %>%
  dplyr::summarise(
    dplyr::across(where(is.numeric), sum),
    dplyr::across(where(function(x) !is.numeric(x)), dplyr::first)
  ) %>%
  dplyr::select(dplyr::all_of(names(ag))) %>%
  as.data.frame(.) %>%
  vm_reformat(verbose = FALSE)

}
