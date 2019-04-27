rm(list = ls())
devtools::load_all()

test <- read_gt3x(
  "data-raw/3x-error/FLPAY002S1D2LW (2017-01-28).gt3x",
  "UTC", TRUE, TRUE, c(
    "METADATA", "PARAMETERS",
    "SENSOR_SCHEMA", "ACTIVITY2"
  )
)

test_raw <- test$RAW
class(test_raw) <- "data.frame"

test2 <- read_AG_raw(
  "data-raw/3x-error/FLPAY002S1D2LW (2017-01-28)RAW.csv",
  verbose = TRUE, return_raw = TRUE
)

nrow(test_raw) - nrow(test2)

test_raw <- test_raw[seq(nrow(test2)), ]
names(test2) <- gsub(" ", "_", names(test2))
test2 <- data.frame(test2, stringsAsFactors = FALSE, row.names = NULL)

test2 <- test2[ ,names(test_raw)]

all.equal(test_raw, test2)
