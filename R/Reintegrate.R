#' Reintegrate a data stream
#'
#' @param ag a data frame to reintegrate
#' @param to the desired epoch length of the output. Starting epoch length will
#'   be determined automatically (while also verifying the data are continuous).
#' @param time_var The name of the column containing POSIX-formatted timestamp
#'   information
#' @param direction The direction of reintegration, i.e. whether a timestamp
#'   refers to the timespan after the previous data point ("backwards"), or
#'   before the next data point ("forwards").
#' @param verbose logical. Print updates to console?
#'
#' @export
#'
#' @examples
#'
#' test_file <- system.file(
#'   "extdata", "example1sec.csv", package = "AGread"
#' )
#'
#' ag <- read_AG_counts(test_file, skip = 11)
#'
#' # Forwards reintegration
#'   reintegrate(
#'     ag = ag,
#'     to = 60,
#'     time_var = "Timestamp",
#'     direction = c("forwards")
#'   )
#'
#' # Backwards reintegration
#'   reintegrate(
#'     ag = ag,
#'     to = 60,
#'     time_var = "Timestamp",
#'     direction = c("backwards")
#'   )
#' \dontrun{
#' # Erronious usages that will give a warning
#'   reintegrate(
#'     ag = ag,
#'     to = 60,
#'     time_var = "Timestamp",
#'     direction = c("forwards", "backwards")
#'   )
#'
#'   reintegrate(
#'     ag = ag,
#'     to = 60,
#'     time_var = "Timestamp"
#'   )
#' }
#'
reintegrate <- function(ag, to, time_var = "Timestamp",
  direction = c("forwards", "backwards"), verbose = FALSE
) {

  ## Initial setup

    setup <- reintegrate_setup(
      ag, to, time_var, direction, verbose
    )
    if (is.null(setup$start_epoch)) return(ag)

  ## Establish the reintegrated epoch groupings

    ag %<>% get_blocks(
      time_var, to, setup$start_epoch, setup$direction
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
