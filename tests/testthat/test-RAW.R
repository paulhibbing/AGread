# Setup -------------------------------------------------------------------

  raw_1s <-  suppressMessages(
    read_AG_raw(
      system.file(
        "extdata", "exampleRAW.csv", package = "AGread"
      )
    )[ ,-2]
  )

  raw_5s <- suppressMessages(read_AG_raw(
    system.file(
      "extdata", "exampleRAW.csv", package = "AGread"
    ),
    5
  )[ ,-2])


# Testing -----------------------------------------------------------------

  test_that("File reading produces expected output", {

    save_raw <- function(code) {
      path <- tempfile(fileext = ".rds")
      saveRDS(code, path)
      path
    }

    testthat::expect_snapshot_file(
      save_raw(raw_1s),
      "RAW_read_cache_1.rds"
    )

    testthat::expect_snapshot_file(
      save_raw(raw_5s),
      "RAW_read_cache_5.rds"
    )

  })
