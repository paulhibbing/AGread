#' One-dimensional linear interpolation
#'
#' @param from starting value
#' @param to ending value
#' @param ... further arguments passed to \code{seq}
#'
#' @keywords internal
#'
seq_interpolate <- function(from, to, ...) {
  value <- seq(from, to, ...)
  value[-c(1, length(value))]
}

#' Perform linear interpolation to fill in SENSOR_DATA payloads that are missing
#' one or more samples
#'
#' @param record_set data frame. Information about the SENSOR_DATA packets (one
#'   row per packet)
#' @inheritParams read_record
#' @param verbose logical. Print updates to console?
#'
#' @keywords internal
#'
interpolate_sensor_records <- function(record_set, schema, verbose) {

  # record_set <- records

  ## Check if interpolation is necessary
  # n_to_interpolate <- sum(
  #   sapply(
  #     records,
  #     function(x) max(x$Payload$interpolate),
  #     USE.NAMES = FALSE
  #   )
  # )
  n_to_interpolate <- sum(record_set$Result.interpolate)
  if (n_to_interpolate == 0) return(record_set)

  stop(paste(
    "Interpolation is broken and may not be",
    "proper or necessary."
  ))

  ## Get set up if so
  if (verbose) cat(
    "\r  Interpolating values for", n_to_interpolate,
    "missing SENSOR_DATA samples"
  )
  record_set <- data.frame(record_set, stringsAsFactors = FALSE)
  record_set$index <- seq(nrow(record_set))

  ## Initialize data frame to populate with interpolated values
  interps <- record_set[
    record_set$Result.interpolate != 0,
    ]
  new_values <- matrix(
    NA,
    nrow = sum(interps$Result.interpolate),
    ncol = ncol(interps)
  )
  new_values <- stats::setNames(
    data.frame(new_values),
    names(interps)
  )

  class(new_values$Timestamp) <- class(
    interps$Timestamp
  )
  new_values$Type <- interps$Type[1]

  ## Populate the data frame
  for (i in seq(nrow(interps))) {
    # i <- 1
    new_indices <- sum(
      interps$Result.interpolate[seq(0, i - 1)]
    ) + 1
    new_indices <- seq(
      new_indices,
      new_indices + (interps$Result.interpolate[i] - 1)
    )
    from <- interps$index[i]
    to <- from + 1
    length_out <- 2 +
      interps$Result.interpolate[i]

    new_values$Timestamp[new_indices] <-
      interps$Timestamp[i]

    cols_to_update <- setdiff(
      names(new_values),
      c("Timestamp", "Type")
    )

    for (col_name in cols_to_update) {
      # col_name <- cols_to_update[1]
      new_values[new_indices, col_name] <- do.call(
        c,
        mapply(
          seq_interpolate,
          from = record_set[from, col_name],
          to = record_set[to, col_name],
          length.out = length_out,
          SIMPLIFY = FALSE
        )
      )
    }

  }

  ## Assemble and format the new data set with interpolated values
  record_set <- rbind(record_set, new_values)
  record_set <- record_set[order(record_set$index), ]
  record_set$Result.interpolate <- NULL
  record_set$index <- NULL

  if (verbose) cat("  ............. COMPLETE")
  return(record_set)
}
