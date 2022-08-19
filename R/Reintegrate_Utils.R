#' Remove trailing rows with missing values from data frame
#'
#' @param ag A data frame on which to perform the operation
#'
#' @keywords internal
#'
rm_trail_na <- function(ag) {
  missing <- apply(ag, 1, function(x) any(is.na(x)))
  indices <- rev(which(missing))
  if (indices[1] != nrow(ag)) {
    return(ag)
  }

  consecutive <-
    abs(diff(indices)) == 1

  total_trail <- which(!consecutive)[1]
  last_row <- nrow(ag) - total_trail

  return(ag[seq(last_row), ])
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
#' #  check_epoch(ag, to, time_var, verbose)
#' @keywords internal
check_epoch <- function(ag, to, time_var, verbose) {

  get_epoch(ag, time_var) %T>%
  {stopifnot(
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

#' @rdname reintegrate
#' @usage
#' #  reintegrate_setup(ag, to, time_var = "Timestamp",
#' #    direction = c("forwards", "backwards"), verbose = FALSE)
#' @keywords internal
reintegrate_setup <- function(
  ag, to, time_var = "Timestamp",
  direction = c("forwards", "backwards"), verbose = FALSE
) {

  col_classes <- sapply(ag, function(x) class(x)[1])
  col_numeric <- col_classes %in% c("numeric", "integer")

  list(

    direction = validate_direction(direction),

    start_epoch = check_epoch(
      ag, to, time_var, verbose
    ),

    char_vars = names(col_classes)[!col_numeric],

    num_vars = names(col_classes)[col_numeric],

    tz = lubridate::tz(ag[ ,time_var])

  )

}

#' @rdname get_blocks
#' @param timestamp POSIX scalar
#' @keywords internal
test_second <- function(timestamp, to) {
  lubridate::floor_date(timestamp, "1 min") %>%
  {timestamp - .} %>%
  as.numeric(.) %>%
  {. %% to == 0}
}

#' @rdname get_blocks
#' @param block_size The number of rows of data included in each reintegrated
#'   epoch
#' @param begin A starting index
#' @keywords internal
forwards_start <- function(
  ag, time_var, to, block_size, begin = 1
) {

  while (!test_second(ag[begin, time_var], to)) {
    begin %<>% {. + 1}
  }

  begin

}

#' @rdname get_blocks
#' @keywords internal
backwards_start <- function(
  ag, time_var, to, block_size, begin = 1
) {

  while(!all(
    test_second(ag[begin, time_var], to),
    begin - block_size + 1 > 0
  )) {
    begin %<>% {. + 1}
  }

  begin - block_size + 1

}

#' Assign blocks to a data stream for reintegration
#'
#' @inheritParams reintegrate
#' @param start_epoch The initial epoch length of the data being reintegrated
#'
#' @keywords internal
get_blocks <- function(
  ag, time_var, to, start_epoch, direction
) {

  block_size <- to / start_epoch

  ag <-
    switch(
      direction,
      "forwards"  = forwards_start(ag, time_var, to, block_size),
      "backwards" = backwards_start(ag, time_var, to, block_size)
    ) %>%
    setdiff(1:., .) %>%
    {if (!length(.)) ag else ag[-., ]}

  while ( (nrow(ag)*start_epoch) %% to != 0 ) {
    ag %<>% {.[-nrow(.), ]}
  }

  ag$block <-
    nrow(ag) %>%
    {. / block_size} %T>%
    {stopifnot(. %% 1 == 0)} %>%
    seq(.) %>%
    rep(each = block_size) %T>%
    {stopifnot(length(.) == nrow(ag))}

  ag

}

#' Run a reintegration operation on a data frame with a \code{block} indicator
#'
#' @param ag data frame with a \code{block} column to indicate groupings for the
#'   reintegration operation
#' @param input_vars character vector of column names on which to reintegrate
#' @param fun the function(s) to perform
#'
#' @keywords internal
reint_wrap <- function(ag, input_vars, fun) {

  c(input_vars, "block") %>%
  unique(.) %>%
  ag[ ,.] %>%
  dplyr::group_by(block) %>%
  dplyr::summarise_all(fun) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  .[ ,input_vars]

}

vm_reformat <- function(ag, verbose) {

  if (all(.triaxial_vars %in% names(ag))) {
    ag$Vector.Magnitude <-
      ag[ ,.triaxial_vars] %>%
      get_VM("Rcpp", verbose) %>%
      round(2)
  }

  ag

}
