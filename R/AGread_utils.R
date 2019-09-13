#' Calculate vector magnitude
#'
#' @param triaxial a dataframe of triaxial data on which to calculate vector magnitude
#' @param verbose print information about variable search criteria?
#'
#' @examples
#' data(imu_to_collapse)
#'
#' vm_columns <-
#'     grepl("accelerometer",
#'         names(imu_to_collapse),
#'         ignore.case = TRUE)
#'
#' get_VM(data.frame(imu_to_collapse)[, vm_columns])
#'
#' @return a vector of vector magnitude values
#' @export
get_VM <- function(triaxial, verbose = FALSE) {
  if (verbose) {
    vm_variables <-
      gsub("\"", "", substring(deparse(substitute(triaxial)), unlist(gregexpr(
        "\"", deparse(substitute(triaxial))
      ))[1],
        unlist(gregexpr(
          "\"", deparse(substitute(triaxial))
        ))[2]))

    if (verbose) message_update(22, vm_variables = vm_variables)
  }
  triaxial <- triaxial[, !grepl("VM", names(triaxial))]
  stopifnot(ncol(triaxial) == 3)
  apply(triaxial, 1, function(x) sqrt(sum(x^2)))
}
