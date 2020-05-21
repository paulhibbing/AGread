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
#' AG_counts <- read_AG_counts(
#'   system.file(
#'   "extdata",
#'   "example1sec.csv",
#'   package = "AGread"
#'   ),
#'   skip = 11
#' )
#' AG_RAW <- read_AG_raw(
#'   system.file(
#'   "extdata",
#'   "exampleRAW.csv",
#'   package = "AGread"
#'   )
#' )
#' AG_IMU <- read_AG_IMU(
#'   system.file(
#'   "extdata",
#'   "example-IMU.csv",
#'   package = "AGread"
#'   )
#' )
#' file_3x <- system.file(
#'   "extdata", "example.gt3x", package = "AGread"
#' )
#' AG_3x <- read_gt3x(file_3x)
#'
#' head(AG_counts)
#' head(AG_RAW)
#' head(AG_IMU)
#' head(lapply(AG_3x, head))
#' }
#'
#' @importFrom magrittr %>% %T>% %<>% %$%
#' @importFrom rlang := .data
#' @import Rcpp
#' @docType package
#' @useDynLib AGread
#' @name AGread
NULL
