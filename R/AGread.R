#' Read Data Files from ActiGraph Monitors
#'
#' This provides support for reading ActiGraph files of various modes into R.
#' For more information see:
#' \url{https://actigraph.desk.com/customer/en/portal/articles/2515800-what-do-the-different-mode-numbers-mean-in-a-csv-or-dat-file-}.
#' Functions are provided to read and minimally pre-process raw data from
#' primary accelerometer and inertial measurement unit files. Reading binary
#' .gt3x files is now supported as well. See
#' \url{https://github.com/actigraph/GT3X-File-Format} for more information.
#'
#' @section Core functions:
#' \code{\link{read_AG_counts}}
#'
#' \code{\link{read_AG_raw}}
#'
#' \code{\link{read_AG_IMU}}
#'
#' \code{\link{read_gt3x}}
#'
#' @examples
#' \donttest{
#' read_AG_counts(
#'   system.file(
#'   "extdata",
#'   "example1sec.csv",
#'   package = "AGread"
#'   ),
#'   skip = 11
#' )
#' read_AG_raw(
#'   system.file(
#'   "extdata",
#'   "TestID_LeftWrist_RAW.csv",
#'   package = "AGread"
#'   )
#' )
#' read_AG_IMU(
#'   system.file(
#'   "extdata",
#'   "example-IMU.csv",
#'   package = "AGread"
#'   )
#' )
#' file_3x <- system.file(
#'   "extdata", "example.gt3x", package = "AGread"
#' )
#' read_gt3x(file_3x)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @docType package
#' @name AGread
NULL
