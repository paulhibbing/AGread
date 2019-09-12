#' Wrapper for \code{\link{zero_pad}} used to fix data files when the device
#' was connected to USB
#'
#' @inheritParams zero_pad
#' @param run_break_indices indices at which a break occurs
#'
#' @keywords internal
insert_zero_runs <- function(records, run_break_indices) {

  run_break_indices <- run_break_indices[
    run_break_indices != length(records)
  ]

  if (!length(run_break_indices)) return(records)

  indices <- seq(records)

  run_breaks <- rle(
    indices %in% run_break_indices
  )$lengths

  run_breaks <- cumsum(run_breaks)

  group <- sapply(
    indices,
    function(x) min(
      which(x <= run_breaks)
    )
  )

  records <- split(records, group)

  indices <- match(run_break_indices, run_breaks)

  results <- lapply(
    indices, zero_pad, records = records
  )

  records[[indices]] <- results[[seq_along(indices)]]
  do.call(c, records)

}

