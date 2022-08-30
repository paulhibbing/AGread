# Setup -------------------------------------------------------------------

  test_file <- system.file(
    "extdata", "example1sec.csv", package = "AGread"
  )

  ag <- read_AG_counts(test_file, header = TRUE)

  forward5 <- reintegrate(ag, 5, "Timestamp", "legacy", "forwards")
  forward60 <- reintegrate(forward5, 60, "Timestamp", "legacy", "forwards")
  backward5 <- reintegrate(ag, 5, "Timestamp", "legacy", "backwards")
  backward60 <- reintegrate(backward5, 60, "Timestamp", "legacy", "backwards")

  tidy5 <- reintegrate(ag, 5)
  tidy60 <- reintegrate(ag, 60)


# Testing -----------------------------------------------------------------

  test_that("Reintegration produces expected output", {

    save_reint <- function(code) {
      path <- tempfile(fileext = ".rds")
      saveRDS(code, path)
      path
    }

    testthat::expect_equal(forward5, tidy5)
    testthat::expect_equal(forward60, tidy60)

    testthat::expect_snapshot_file(
      save_reint(forward5),
      "forward5.rds"
    )
    testthat::expect_snapshot_file(
      save_reint(forward60),
      "forward60.rds"
    )
    testthat::expect_snapshot_file(
      save_reint(backward5),
      "backward5.rds"
    )
    testthat::expect_snapshot_file(
      save_reint(backward60),
      "backward60.rds"
    )

  })
