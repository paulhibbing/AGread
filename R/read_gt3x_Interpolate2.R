#' Resample a sensor data stream
#'
#' This function was originally developed to correct sensor packets when sampled
#' more or fewer times than expected over the time interval. However, this
#' function may have more far-reaching applications.
#'
#' @param original_samples numeric. the original data stream
#' @param target_frequency numeric. the target sampling frequency
#' @param method character. Resampling method to apply. Currently accepts only
#'   \code{interpolate}.
#' @param ... additional arguments to \code{method}
#'
#' @return An appropriately up/down-sampled data stream (numeric vector)
#' @export
#'
#' @examples
#' set.seed(14)
#' target_frequency <- 100
#'
#' ## Downsample
#'    original_samples <- sample(
#'      seq(1.3,2.4,0.12), 101, replace = TRUE
#'    )
#'    sensor_resample(original_samples, target_frequency)
#'
#' ## Upsample
#'    original_samples <- original_samples[1:99]
#'    sensor_resample(original_samples, target_frequency)
sensor_resample <- function(
  original_samples, target_frequency,
  method = "interpolate", ...
) {

  method <- match.arg(method)
  switch(
    method,
    "interpolate" = res_interpolate(
      original_samples, target_frequency, ...
    )
  )

}

#' @rdname sensor_resample
#' @keywords internal
res_interpolate <- function(
  original_samples, target_frequency, ...
) {

  intervals <- seq(0, 1, along.with = original_samples)

  ## Interpolation information
    prop_min <- intervals[-length(intervals)]
    reference_frame <- data.frame(
      interval = cut(
        prop_min, intervals,
        right = FALSE, include.lowest = TRUE
      ),
      prop_min = prop_min,
      start = original_samples[-length(original_samples)],
      rise = diff(original_samples),
      run = diff(intervals),
      row.names = NULL
    )

    ## New Data
      proportion <- seq(
        0, target_frequency - 1
      )/target_frequency

      index <- cut(
        proportion, intervals,
        right = FALSE, include.lowest = TRUE
      )

      stopifnot(identical(
        levels(index), levels(reference_frame$interval)
      ))

      index <- as.numeric(index)

      window_fraction <- (
        proportion - reference_frame$prop_min[index]
      ) / reference_frame$run[index]

      stopifnot(
        all(window_fraction <= 1),
        all(window_fraction >= 0)
      )

      reference_frame$start[index] + (
        reference_frame$rise[index] *
          window_fraction
      )

}
