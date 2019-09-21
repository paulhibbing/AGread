# Setup -------------------------------------------------------------------

    rm(list = ls())

  ## Check that AGread v1.0.0 is installed

    stopifnot("AGread" %in% installed.packages())

    expected_version <- structure(
      list(c(1L, 0L, 0L)),
      class = c("package_version", "numeric_version")
    )

    installed_version <- packageVersion("AGread")

    if (!identical(expected_version, installed_version)) {
      stop("It appears you don\'t have AGread v1.0.0 installed.")
    } else {
      rm(expected_version, installed_version)
    }

  ## Check that R can locate the gt3x and RAW.csv files

    file_directory <- getwd()
    file_3x <- file.path(
      file_directory, "TAS1H30182785 (2019-09-17).gt3x"
    )
    file_csv <- file.path(
      file_directory, "TAS1H30182785 (2019-09-17)RAW.csv"
    )

    if (!all(file.exists(c(file_3x, file_csv)))) {
      stop(paste(
        "Can\'t find the data files. Try adjusting the",
        "\n  value of `file_directory` object."
      ))
    } else {
      rm(file_directory)
      library(AGread)
    }

# Read Files and prepare for comparison -----------------------------------

    all_3x <- read_gt3x(file_3x, verbose = TRUE)
    test_3x <- all_3x$RAW
    class(test_3x) <- "data.frame"

    test_RAW <- read_AG_raw(
      file_csv, return_raw = TRUE, verbose = TRUE
    )
    test_RAW <- test_RAW[ ,names(test_3x)]

# Compare the output of both files ----------------------------------------

    accel_names <- setdiff(names(test_3x), "Timestamp")
    
    PAutilities::test_errors(
      test_3x, test_RAW, accel_names,
      return_logical = FALSE
    )
    
    all.equal(test_3x, test_RAW, scale = 1) # Timestamps are OK
    
    sapply(
      accel_names,
      function(x) summary(
      abs(test_3x[ ,x] - test_RAW[ ,x])
      ),
      simplify = FALSE
    )
    