.triaxial_vars <- c("Axis1", "Axis2", "Axis3")

#' @rdname reintegrate
#' @usage
#' #  reintegrate_setup(input)
#' @keywords internal
reintegrate_setup <- function(
  ag, to, time_var = "Timestamp",
  direction = c("forwards", "backwards"), verbose = FALSE,
  method = c("Rcpp", "legacy")
) {

  col_classes <- sapply(ag, function(x) class(x)[1])
  col_numeric <- col_classes %in% c("numeric", "integer")

  list(

    method = match.arg(method),

    direction = validate_direction(direction),

    start_epoch = get_epoch(
      ag, to, time_var, verbose
    ),

    first_vars = names(col_classes)[!col_numeric],

    sum_vars = names(col_classes)[col_numeric],

    tz = lubridate::tz(ag[ ,time_var])

  )

}

#' Remove trailing rows with missing values from data frame
#'
#' @param ag A data frame on which to perform the operation
#'
#' @keywords internal
#'
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

#' Assign blocks to data stream for reintegration
#'
#' @inheritParams reintegrate
#' @param start_epoch The initial epoch length of the data being reintegrated
#' @param block_size The number of rows of data included in each reintegrated
#'   epoch
#'
#' @keywords internal
#'
get_blocks <- function(ag, time_var, to, start_epoch, block_size, direction) {

  if (direction == "forwards") {

    begin <-
      ag[ ,time_var] %>%
      as.numeric(.) %>%
      {. %% to == 0} %>%
      which(.) %>%
      .[1]

  }

  if (direction == "backwards") {

    begin <-
      ag[ ,time_var] %>%
      as.numeric(.) %>%
      {. %% to == 0} %>%
      which(.) %>%
      .[2] %>%
      {. - (block_size - 1)}

    ag %<>%
      nrow(.) %>%
      seq(.) %>%
      {. >= begin} %>%
      ag[., ]

    begin <- 1

  }

  new_block <-
    ag[begin, time_var] %>%
    as.numeric(.) %>%
    {. %% to}

  block_no  <-
    ag[ ,time_var] %>%
    as.numeric(.) %>%
    {. %% to == new_block} %>%
    cumsum(.)

  sizes <- tapply(block_no, block_no, length)
  keep  <- sizes == block_size

  ag$block_no   <- block_no
  ag$block_size <- sizes[match(block_no, names(sizes))]
  ag$keep       <- keep[match(block_no, names(keep))]

  ag            <- ag[ag$keep, ]
  ag$keep       <- NULL
  ag$block_size <- NULL
  return(ag)
}
