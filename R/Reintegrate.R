#' Reintegrate ActiGraph data
#'
#' @param ag a data frame to reintegrate
#' @param target_sec the desired epoch length of the output. Starting epoch
#'   length will be determined automatically
#' @param time_var The name of the column containing POSIX-formatted timestamp
#'   information
#' @param method character scalar indicating the desired method of
#'   reintegration. Options are \code{tidy} (the default) and \code{legacy}. See
#'   details
#' @param ... arguments passed to \code{reintegrate_legacy}
#' @param direction The direction of reintegration, i.e. whether a timestamp
#'   refers to the timespan after the previous data point ("backwards"), or
#'   before the next data point ("forwards").
#' @param verbose logical. Print updates to console?
#'
#' @details
#'
#' Two methods are provided. One is a legacy method that allows "forward" or
#' "backward" reintegration, depending on whether the timestamp occurs
#' (respectively) at the start of the interval (typical for activity monitors)
#' or the end (typical for indirect calorimeters). The other is a \code{tidy}
#' approach, which is both more straightforward and more concise. However,
#' runtime implications are uncertain, and only forward reintegration is
#' supported (consistent with ActiLife precedent).
#'
#'
#' @export
#'
#' @examples
#'
#' test_file <- system.file(
#'   "extdata", "example1sec.csv", package = "AGread"
#' )
#'
#' ag <- read_AG_counts(test_file, header = TRUE)
#'
#' # Old Method:
#'
#'   old_result <- reintegrate(
#'     ag = ag,
#'     target_sec = 60,
#'     time_var = "Timestamp",
#'     direction = c("forwards")
#'   )
#'
#' # New Method:
#'
#'   new_result <- reintegrate(ag, 60)
#'
reintegrate <- function(
  ag, target_sec, time_var = "Timestamp",
  method = c("tidy", "legacy"), ...
) {

  method <- match.arg(method)

  if (method == "legacy") return(
    reintegrate_legacy(ag, target_sec, time_var, ...)
  )

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


# Legacy method -----------------------------------------------------------

#' @rdname reintegrate
reintegrate_legacy <- function(
  ag, target_sec, time_var = "Timestamp",
  direction = c("forwards", "backwards"), verbose = FALSE
) {

  ## Initial setup

  setup <- reintegrate_setup(
    ag, target_sec, time_var, direction, verbose
  )
  if (is.null(setup$start_epoch)) return(ag)

  ## Establish the reintegrated epoch groupings

  ag %<>% get_blocks(
    time_var, target_sec, setup$start_epoch, setup$direction
  )

  ## Run the reintegration operations

  ag <-
    switch(
      setup$direction,
      "forwards" = dplyr::first,
      "backwards" = dplyr::last
    ) %>%
    {mapply(
      reint_wrap,
      input_vars = list(setup$char_vars, setup$num_vars),
      fun = list(., sum),
      MoreArgs = list(ag = ag),
      SIMPLIFY = FALSE
    )} %>%
    data.frame(stringsAsFactors = FALSE, row.names = NULL)

  ## Re-format VM and return

  vm_reformat(ag, verbose = verbose)

}
