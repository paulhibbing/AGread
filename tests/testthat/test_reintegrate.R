context("Reintegration")
library(AGread)

testthat::test_that("Reintegration produces expected output", {

  test_file <- system.file(
    "extdata", "example1sec.csv", package = "AGread"
  )

  ag <- read_AG_counts(test_file, skip = 11)

  forward5 <- reintegrate(ag, 5, "Timestamp", "forwards")
  forward60 <- reintegrate(forward5, 60, "Timestamp", "forwards")
  backward5 <- reintegrate(ag, 5, "Timestamp", "backwards")
  backward60 <- reintegrate(backward5, 60, "Timestamp", "backwards")

  testthat::expect_equal_to_reference(forward5, "forward5.rds")
  testthat::expect_equal_to_reference(forward60, "forward60.rds")
  testthat::expect_equal_to_reference(backward5, "backward5.rds")
  testthat::expect_equal_to_reference(backward60, "backward60.rds")

})
