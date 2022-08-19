f <- system.file("extdata", "example1sec.csv", package = "AGread")

testthat::test_that("csv file reading produces expected output", {

  save_counts <- function(code) {
    path <- tempfile(fileext = ".rds")
    saveRDS(code, path)
    path
  }

  testthat::expect_snapshot_file(
    save_counts(
      read_AG_counts(f)
    ),
    "read_counts.rds"
  )

})
