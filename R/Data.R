#' IMU data to check
#'
#' A dataset for demonstrating checks that are applied to IMU data.
#'
#' @format A data frame with 300 rows and 8 variables:
#' \describe{
#'   \item{file_source_IMU}{The filename of the IMU file}
#'   \item{date_processed_IMU}{The date the IMU file was processed}
#'   \item{Timestamp}{The corresponding time for each row of data}
#'   \item{Gyroscope_VM_DegPerS}{Gyroscope vector magnitude, in degrees per
#'   second}
#'   \item{mean_abs_Gyroscope_x_DegPerS}{Rotation in x axis, degrees per second}
#'   \item{mean_abs_Gyroscope_y_DegPerS}{Rotation in y axis, degrees per second}
#'   \item{mean_abs_Gyroscope_z_DegPerS}{Rotation in z axis, degrees per second}
#'   \item{mean_magnetometer_direction}{Cardinal direction of magnetometer
#'   signal, averaged over one second}
#' }
"imu_to_check"

#' IMU data to collapse
#'
#' A partially-processed IMU dataset ready to be collapsed from raw samples to
#' one-second summaries.
#'
#' @format A data frame with 1500 rows and 17 variables:
#' \describe{
#'   \item{Timestamp}{The corresponding time for each row of data}
#'   \item{Accelerometer.X}{Secondary accelerometer x-axis data, in G}
#'   \item{Accelerometer.Y}{Secondary accelerometer y-axis data, in G}
#'   \item{Accelerometer.Z}{Secondary accelerometer z-axis data, in G}
#'   \item{Temperature}{Temperature of the IMU, in Celcius}
#'   \item{Gyroscope.X}{Gyroscope x-axis data, in degrees per second}
#'   \item{Gyroscope.Y}{Gyroscope y-axis data, in degrees per second}
#'   \item{Gyroscope.Z}{Gyroscope z-axis data, in degrees per second}
#'   \item{Magnetometer.X}{Magnetometer x-axis data, in micro-Teslas}
#'   \item{Magnetometer.Y}{Magnetometer y-axis data, in micro-Teslas}
#'   \item{Magnetometer.Z}{Magnetometer z-axis data, in micro-Teslas}
#'   \item{file_source_IMU}{The filename of the IMU file}
#'   \item{date_processed_IMU}{The date the IMU file was processed}
#'   \item{ms}{The millisecond value of the timestamp}
#'   \item{mean_Accel_VM}{Vector magnitude of the secondary accelerometer
#'   signal, in G}
#'   \item{Gyroscope_VM_DegPerS}{Gyroscope vector magnitude, in degrees per
#'   second}
#'   \item{Magnetometer_VM_MicroT}{Vector magnitude of the magnetometer signal,
#'   in micro-Teslas}
#' }
"imu_to_collapse"

#' Primary accelerometer data to collapse
#'
#' A partially-processed primary accelerometer dataset ready to be collapsed
#' from raw samples to one-second summaries.
#'
#' @format A data frame with 24000 rows and 3 variables:
#' \describe{
#'   \item{Accelerometer X}{Primary accelerometer x-axis data, in G}
#'   \item{Accelerometer Y}{Primary accelerometer y-axis data, in G}
#'   \item{Accelerometer Z}{Primary accelerometer z-axis data, in G}
#' }
"raw_to_collapse"
