#' Read a large raw file in blocks
#'
#' @inheritParams read_AG_raw
#' @param meta information from \code{\link{get_raw_file_meta}}
#' @param timer initial time passed from initial call to \code{\link{read_AG_raw}}
#'
#' @keywords internal
#'
read_AG_raw_block <- function(file, output_window_secs, verbose, skip,
  meta, timer, ...) {

  # Initialize variables for the loop
    block_size <- meta$samp_freq * 86400
    AG <- data.frame()
    i <- 0

    ag_names <- names(
      data.table::fread(file, nrows = 1, stringsAsFactors = FALSE,
      showProgress = FALSE, skip = skip)
    )
    ag_names <- gsub("\\.", " ", ag_names)

    ENMO2 <- NULL
    stopper <- FALSE

  repeat {

    new_skip <- skip + (i * block_size)
    if (i > 0) new_skip <- new_skip + 1
    new_data <- data.table::fread(file, stringsAsFactors = FALSE,
      showProgress = FALSE, skip = new_skip, nrows = block_size)

    if (nrow(new_data) != block_size) stopper <- TRUE

    names(new_data) <- ag_names
    new_data <- AG_collapse(new_data, output_window_secs,
      meta$samp_freq, method = "block", ENMO2 = ENMO2)

    AG <- rbind(AG, new_data$AG)
    ENMO2 <- new_data$ENMO2

    if (stopper) break

    i <- i + 1

  }

  block_size <- (meta$samp_freq * output_window_secs)
  AG <- data.frame(
    Block = seq(nrow(AG) - 1),
    ENMO = (diff(AG$ENMO) / block_size) * 1000
  )

  return(AG)
}
