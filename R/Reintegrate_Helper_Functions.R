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
    begin <- which(as.numeric(ag[ ,time_var]) %% to == 0)[1]
  }

  if (direction == "backwards") {
    begin <- which(as.numeric(ag[ ,time_var]) %% to == 0)[2]
    begin <- begin - (block_size - 1)
    keep <- seq(nrow(ag)) >= begin
    ag <- ag[keep, ]

    begin <- 1
  }

  new_block <- (as.numeric(ag[begin ,time_var]) %% to)
  block_no  <- cumsum(as.numeric(ag[ ,time_var]) %% to == new_block)

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
