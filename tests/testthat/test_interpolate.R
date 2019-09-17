context("Interpolation")
library(AGread)

testthat::test_that("Interpolation give expected values", {

  # set.seed(14)
  # original_samples <- sample(
  #   seq(1.3,2.4,0.12), 101, replace = TRUE
  # )
  target_frequency <- 100
  original_samples <- c(
    1.54, 2.02, 2.38, 1.9, 2.38, 1.9,
    2.38, 1.78, 1.78, 1.66, 2.26, 1.42,
    1.78, 2.26, 2.26, 2.14, 1.66, 2.02,
    2.26, 1.9, 1.66, 1.78, 2.02, 2.02,
    2.14, 2.02, 1.66, 1.9, 2.02, 1.3,
    2.26, 1.42, 2.38, 2.02, 2.38, 1.42,
    1.9, 1.78, 2.26, 2.02, 1.42, 2.38,
    1.78, 1.78, 1.9, 1.54, 1.66, 1.54,
    2.14, 2.02, 1.54, 1.42, 2.38, 1.3,
    2.26, 1.42, 1.66, 2.14, 1.3, 1.9,
    1.3, 2.02, 1.66, 1.78, 2.02, 1.42,
    2.14, 2.26, 2.02, 1.78, 1.66, 2.14,
    2.02, 2.02, 1.66, 2.14, 2.38, 2.02,
    1.9, 2.02, 1.66, 1.42, 2.02, 1.78,
    1.9, 2.26, 1.78, 1.42, 1.3, 1.54,
    1.3, 1.42, 1.42, 2.26, 2.14, 2.38,
    2.14, 1.3, 1.78, 2.14, 1.3
  )

  down_R <- sensor_resample(
    original_samples, target_frequency
  )
  down_C <- sensor_resample(
    original_samples, target_frequency,
    "linear_C"
  )
  testthat::expect_equal(down_R, down_C)

  up_R <- sensor_resample(
    original_samples[1:99], target_frequency
  )
  up_C <- sensor_resample(
    original_samples[1:99], target_frequency,
    "linear_C"
  )
  testthat::expect_equal(up_R, up_C)

  up_IMU <- sensor_resample(
    original_samples, target_frequency,
    "IMU"
  )
  down_IMU <- sensor_resample(
    original_samples[1:99], target_frequency,
    "IMU"
  )

  results <- data.frame(
    up_R, up_C, up_IMU,
    down_R, down_C, down_IMU
  )

  testthat::expect_equal_to_reference(
    results, "interpolate_cache.rds"
  )

})
