context("Reintegration")
library(AGread)

testthat::test_that("Reintegration produces expected output", {

  test_file <- system.file(
    "extdata", "example1sec.csv", package = "AGread"
  )

  ag <- read_AG_counts(test_file, skip = 11)

  forward5 <- reintegrate(ag, 5, "Timestamp", "forwards")
  row.names(forward5) <- as.character(row.names(forward5))
  forward60 <- reintegrate(forward5, 60, "Timestamp", "forwards")
  row.names(forward60) <- as.character(row.names(forward60))
  backward5 <- reintegrate(ag, 5, "Timestamp", "backwards")
  row.names(backward5) <- as.character(row.names(backward5))
  backward60 <- reintegrate(backward5, 60, "Timestamp", "backwards")
  backward60 <- backward60[-1, ]
  row.names(backward60) <- seq(nrow(backward60))

  testthat::expect_equal_to_reference(forward5, "forward5.rds")
  testthat::expect_equal_to_reference(forward60, "forward60.rds")
  testthat::expect_equal_to_reference(backward5, "backward5.rds")
  testthat::expect_equal_to_reference(backward60, "backward60.rds")

})
