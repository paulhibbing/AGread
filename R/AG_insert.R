#' Calculate a new variable and insert it in a data frame
#'
#' @param AG The original data frame
#' @param var_name character. The new variable name
#' @param after_var character. The existing variable name, after which to insert
#'   the new variable
#' @param value The value to assign the new variable
#'
#' @return A data frame with the new variable added at the specified index
#' @export
#'
#' @examples
#'
#' data("imu_to_collapse")
#' test <- imu_to_collapse[ ,1:3]
#' head(AG_insert(
#'   test, "new_variable",
#'   "Timestamp", "A new variable"
#' ))
AG_insert <- function(AG, var_name, after_var, value) {
  stopifnot(after_var %in% names(AG))
  index <- which(names(AG) == after_var)
  new_names <- append(names(AG), var_name, index)
  AG[ ,var_name] <- value
  AG[ ,new_names]
}
