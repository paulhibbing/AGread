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

  method <- match.arg(method)
  direction %<>% validate_direction(.)

  start_epoch <- get_epoch(ag, to, time_var, verbose)
  if (is.null(start_epoch)) return(ag)

  col_classes <- sapply(ag, function(x) class(x)[1])
  col_numeric <- col_classes %in% c("numeric", "integer")
  first_vars  <- names(col_classes)[!col_numeric]
  sum_vars    <- names(col_classes)[col_numeric]
  tz <- lubridate::tz(ag[ ,time_var])

  ag <-
    (to / start_epoch) %>%
    get_blocks(ag, time_var, to, start_epoch, ., direction)

  ag[ ,time_var] %<>% as.character(.)

  firsts  <-
    names(ag) %>%
    match(first_vars, .) %>%
    sapply(
      function(x) tapply(
        ag[ ,x], ag$block_no, function(y) switch(
          direction,
          "forwards" = y[1],
          "backwards" = y[length(y)]
        )
      )
    ) %>%
    {if (is.null(dim(.))) t(.) else .} %>%
    data.frame(stringsAsFactors = FALSE) %>%
    stats::setNames(first_vars)

  sums <-
    names(ag) %>%
    match(sum_vars, .) %>%
    sapply(function(x) tapply(
      ag[ ,x], ag$block_no, sum, na.rm = TRUE
    )) %>%
    {if (is.null(dim(.))) t(.) else .} %>%
    data.frame(stringsAsFactors = FALSE) %>%
    stats::setNames(sum_vars)

  ag <- data.frame(firsts, sums, stringsAsFactors = FALSE)
  ag[ ,sum_vars] %<>% sapply(as.numeric)
  ag[ ,time_var] %<>% as.POSIXct(tz)
  ag$block_no <- NULL

  if (all(.triaxial_vars %in% names(ag))) {
    ag$Vector.Magnitude <-
      ag[ ,.triaxial_vars] %>%
      get_VM(method, verbose) %>%
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
