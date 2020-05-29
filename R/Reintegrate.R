#' Reintegrate a data stream
#'
#' @param ag A data frame to reintegrate
#' @param to The epoch length desired. Starting epoch length will be determined
#'   automatically.
#' @param time_var The name of the column containing POSIX-formatted timestamp
#'   information
#' @param direction The direction of reintegration, i.e. whether a timestamp
#'   refers to the timespan after the previous data point ("backwards"), or
#'   before the next data point ("forwards").
#' @param verbose logical. Print updates to console?
#' @param method Method to use for vector magnitude calculations, namely Rcpp
#'   (default, faster) or legacy (for checking backward compatibility)
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
  direction = c("forwards", "backwards"), verbose = FALSE,
  method = c("Rcpp", "legacy")
) {

  ## Initial setup

    setup <- reintegrate_setup(
      ag, to, time_var, direction, verbose, method
    )
    if (is.null(setup$start_epoch)) return(ag)

  ## Establish the reintegrated epoch groupings

    ag <-
      (to / setup$start_epoch) %>%
      get_blocks(ag, time_var, to, setup$start_epoch, ., setup$direction)

  ag[ ,time_var] %<>% as.character(.)

  firsts  <-
    names(ag) %>%
    match(setup$first_vars, .) %>%
    sapply(
      function(x) tapply(
        ag[ ,x], ag$block_no, function(y) switch(
          setup$direction,
          "forwards" = y[1],
          "backwards" = y[length(y)]
        )
      )
    ) %>%
    {if (is.null(dim(.))) t(.) else .} %>%
    data.frame(stringsAsFactors = FALSE) %>%
    stats::setNames(setup$first_vars)

  sums <-
    names(ag) %>%
    match(setup$sum_vars, .) %>%
    sapply(function(x) tapply(
      ag[ ,x], ag$block_no, sum, na.rm = TRUE
    )) %>%
    {if (is.null(dim(.))) t(.) else .} %>%
    data.frame(stringsAsFactors = FALSE) %>%
    stats::setNames(setup$sum_vars)

  ag <- data.frame(firsts, sums, stringsAsFactors = FALSE)
  ag[ ,setup$sum_vars] %<>% sapply(as.numeric)
  ag[ ,time_var] %<>% as.POSIXct(setup$tz)
  ag$block_no <- NULL

  if (all(.triaxial_vars %in% names(ag))) {
    ag$Vector.Magnitude <-
      ag[ ,.triaxial_vars] %>%
      get_VM("Rcpp", verbose) %>%
      round(2)
  }

  return(ag)

}

#' @rdname reintegrate
#' @usage
#' ## Related internal functions:
#'
#' #  validate_direction(direction)
#' @keywords internal
validate_direction <- function(direction) {

  direction <- try(
    match.arg(
      direction, c("err", "forwards", "backwards"), FALSE
    ),
    silent = TRUE
  )

  if (class(direction) == "try-error" | direction == "err") {
    warning(paste("Argument `direction` must be exactly one of",
      "\"forwards\" or \"backwards\". Defaulting to forwards."))
    direction <- "forwards"
  }

  direction

}

#' @rdname reintegrate
#' @usage
#' #  get_epoch(ag, to, time_var, verbose)
#' @keywords internal
get_epoch <- function(ag, to, time_var, verbose) {

  ag[ ,time_var] %>%
    diff(.) %>%
    unique(.) %T>%
    {stopifnot(
      length(.) == 1,
      (to / .) %% 1 == 0
    )} %>%
    {if (. == to) {
      if (verbose) cat(
        "\nReturning original data --",
        "already in desired epoch length"
      )
      NULL
    } else {
      .
    }}

}
