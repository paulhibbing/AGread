context("Interpolation")
library(AGread)

testthat::test_that("Interpolation give expected values", {

  set.seed(14)
  target_frequency <- 100
  original_samples <- sample(
    seq(1.3,2.4,0.12), 101, replace = TRUE
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
