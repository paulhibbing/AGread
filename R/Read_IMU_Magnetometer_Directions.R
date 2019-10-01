#' Convert magnetometer signal to cardinal direction
#'
#' @param x x-axis magnetometer data
#' @param y y-axis magnetometer data
#' @param z z-axis magnetometer data
#' @param orientation the conversion scheme to use, from c("vertical",
#'   "horizontal")
#'
#' @return A vector of cardinal directions assigned from the set
#'     {N, NNE, NE, ENE, E, ESE, SE, SSE, S,
#'      SSW, SW, WSW, W, WNW, NW, NNW}, where N, E, S, and W are
#'      north, east, south, and west, respectively.
#'
#' @examples
#' data(imu_to_collapse)
#'
#' X <- mean(imu_to_collapse$Magnetometer.X)
#' Y <- mean(imu_to_collapse$Magnetometer.Y)
#' Z <- mean(imu_to_collapse$Magnetometer.Z)
#'
#' classify_magnetometer(X, Y, Z)
#'
#' @seealso
#'   \url{http://s3.amazonaws.com/actigraphcorp.com/wp-content/uploads/2017/11/26205750/ActiGraph_IMU_White_Paper.pdf}
#'
#' @export
classify_magnetometer <- function(
  x = "Magnetometer X", y = "Magnetometer Y",
  z = "Magnetometer Z", orientation = c("vertical", "horizontal")
) {

  orientation <- match.arg(orientation)

  if (length(x) != length(y)) {
    message_update(26, is_message = TRUE)
    return(NULL)
  }

  n <- length(x)

  if (length(x) > 1 | length(y) > 1) {
    x <- mean(x)
    y <- mean(y)
    message_update(27, n = n, is_message = TRUE)
  }

  ## Calculate direction for vertical orientation

    if (orientation == "vertical") {

      zdir <- as.character(cut(z,
        c(
          -Inf, -22, -16.71, -11.43, -6.14, -0.86,
          4.43, 9.71, 15, Inf
        ),
        c(
          "N", "NNx", "Nx", "xNx", "x", "xSx",
          "Sx", "SSx", "S"
        ),
        right = FALSE
      ))

      direction <- if (grepl("x", zdir, ignore.case = TRUE)) {
        gsub("x", if (x > 22)
          "E" else "W", zdir)
      } else zdir

    }

  ## Calculate direction for horizontal orientation

    if (orientation == "horizontal") {

      xdir <- as.character(cut(x,
        c(
          -Inf, -6, -0.29, 5.43, 11.14,
          16.86, 22.57, 28.29, 34, Inf
        ),
        c(
          "N", "NNy", "Ny", "yNy", "y", "ySy",
          "Sy", "SSy", "S"
        ),
        right = FALSE
      ))


      direction <- if (grepl("y", xdir, ignore.case = TRUE)) {
        gsub("y", if (y > 4)
          "E" else "W", xdir)
      } else xdir

    }

  return(rep(direction, n))

}
