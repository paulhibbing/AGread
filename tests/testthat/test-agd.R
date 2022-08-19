# Setup -------------------------------------------------------------------

  agd <- system.file(
    "extdata", "example1sec.agd", package = "AGread"
  )

  csv <- system.file(
    "extdata", "example1sec.csv", package = "AGread"
  )


# Test --------------------------------------------------------------------

  testthat::test_that("agd file reading produces expected output", {

    save_agd <- function(code) {
      path <- tempfile(fileext = ".rds")
      saveRDS(code, path)
      path
    }

    testthat::expect_snapshot_file(
      save_agd(
        read_agd(agd)
      ),
      "read_agd.rds"
    )

    ref <- read_AG_counts(csv)
    test <- read_agd(agd)
    test2 <- read_agd(agd, "settings")
    test3 <- read_agd(agd, "data")
    test4 <- read_agd(agd, "both")

    format_test <- all(
      identical(test, test3),
      identical(test, test4$data),
      identical(test2, test4$settings),
      identical(class(test), class(ref)),
      identical(class(test4$data), class(ref))
    )

    testthat::expect_true(format_test)

    testthat::expect_equal(test, ref)

  })
