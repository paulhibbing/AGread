#' Calculate vector magnitude
#'
#' @param triaxial a dataframe of triaxial data on which to calculate vector
#'   magnitude
#' @param method which method to use, either \code{legacy} (default, for
#'   backwards compatibility) or \code{Rcpp} (gives the same results, faster but
#'   more particular)
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
#' vm_values <- get_VM(
#'   data.frame(imu_to_collapse)[, vm_columns]
#' )
#'
#' head(vm_values)
#'
#' @return a vector of vector magnitude values
#' @export
get_VM <- function(
  triaxial, method = c("legacy", "Rcpp"),
  verbose = FALSE
) {

  method <- match.arg(method)
  if (method == "Rcpp") return(get_VM_C(
    triaxial[ ,1],
    triaxial[ ,2],
    triaxial[ ,3])
  )

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

#' Retrieve the epoch length of an ActiGraph data frame
#'
#' @param AG data frame of ActiGraph data
#' @param time_var character scalar. Name of the time variable column
#'
#' @return scalar epoch length
#' @export
#'
#' @examples
#' AG <- read_AG_counts(
#'   system.file("extdata/example1sec.csv", package = "AGread")
#' )
#' get_epoch(AG)
get_epoch <- function(AG, time_var = "Timestamp") {

  stopifnot(
    inherits(AG, "data.frame"),
    time_var %in% names(AG)
  )

  nrow(AG) %>%
  {. * 0.1} %>%
  ceiling(.) %>%
  pmax(2) %>%
  seq(.) %>%
  AG[.,time_var] %>%
  diff(.) %>%
  as.numeric(units = "secs") %>%
  unique(.) %T>%
  {stopifnot(length(.) == 1)}

}
