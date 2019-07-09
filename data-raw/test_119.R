rm(list = ls())
devtools::load_all()

test <- read_gt3x(
  "data-raw/119AGBPFLW (2016-03-08).gt3x",
  verbose= TRUE
)

## TEST RAW.csv ####

  AG <- test$RAW
  class(AG) <- "data.frame"

  test2 <- read_AG_raw(
    "data-raw/119AGBPFLW (2016-03-08)RAW.csv",
    return_raw = TRUE
  )

  missing_indices <- setdiff(
    seq(nrow(test2)),
    seq(nrow(AG))
  )

  all(sapply(
    test2[missing_indices, -c(1:3)],
    function(x) all(x == 0)
  ))

  test2 <- test2[-missing_indices, ]

  all.equal(
    AG[ ,-1], test2[ ,-c(1:3)],
    tolerance = 0.001, scale = 1
  )

  accel_names <- names(AG)[grepl("Accelerometer", names(AG))]
  sapply(
    accel_names,
    function(x) mean(sqrt((AG[ ,x] - test2[ ,x])^2))
  )

## TEST IMU.csv ####

  AG <- test$IMU
  class(AG) <- "data.frame"

  test2 <- read_AG_IMU(
    "data-raw/119AGBPFLW (2016-03-08)-IMU.csv",
    filter = FALSE, return_raw = TRUE
  )

  missing_indices <- setdiff(
    seq(nrow(test2)),
    seq(nrow(AG))
  )

  all(sapply(
    test2[missing_indices, -c(1:3)],
    function(x) all(x == 0)
  ))

  test2 <- test2[-missing_indices, ]

  all.equal(
    AG[ ,-1], test2[ ,-c(1:3)],
    tolerance = 0.001, scale = 1
  )

  accel_names <- names(AG)[grepl("Accelerometer", names(AG))]
  sapply(
    accel_names,
    function(x) mean(sqrt((AG[ ,x] - test2[ ,x])^2))
  )
