## These functions only exist to make code clearer. Not all PARAMETERS records
## require a unique decoding process, but it's easier to follow if there's a
## decoding function whose name matches the record label


# Alias for `par_battery_voltage` -----------------------------------------

#' @rdname par_battery_voltage
par_board_revision <- function(value, key) {
  # value <- test_val("03 00 00 00")
  # key <- subset(PARAMETERS, Label == "BOARD_REVISION")
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_imu_accel_scale <- function(value, key) {
  # value <- test_val("00 00 40 0C")
  # key <- subset(PARAMETERS, Label == "IMU_ACCEL_SCALE")
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_imu_gyro_scale <- function(value, key) {
  # value <- test_val("37 89 41 05")
  # key <- subset(PARAMETERS, Label == "IMU_GYRO_SCALE")
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_imu_mag_scale <- function(value, key) {
  # value <- test_val("07 3A 6D 03")
  # key <- subset(PARAMETERS, Label == "IMU_MAG_SCALE")
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_accel_scale <- function(value, key) {
  # value <- test_val("00 00 40 09")
  # key <- subset(PARAMETERS, Label == "ACCEL_SCALE")
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_imu_temp_scale <- function(value, key) {
  # value <- test_val("AE 77 53 09")
  # key <- subset(PARAMETERS, Label == "IMU_TEMP_SCALE")
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_imu_temp_offset <- function(value, key) {
  # value <- test_val("00 00 54 05")
  # key <- subset(PARAMETERS, Label == "IMU_TEMP_OFFSET")
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_negative_g_offset_x <- function(value, key) {
  # value <- test_val("49 FF FF FF")
  # key <- subset(PARAMETERS, Label == "NEGATIVE_G_OFFSET_X")
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_negative_g_offset_y <- function(value, key) {
  # value <- test_val("1A FF FF FF")
  # key <- subset(PARAMETERS, Label == "NEGATIVE_G_OFFSET_Y")
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_negative_g_offset_z <- function(value, key) {
  # value <- test_val("52 FF FF FF")
  # key <- subset(PARAMETERS, Label == "NEGATIVE_G_OFFSET_Z")
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_positive_g_offset_x <- function(value, key) {
  # value <- test_val("31 01 00 00")
  # key <- subset(PARAMETERS, Label == "POSITIVE_G_OFFSET_X")
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_positive_g_offset_y <- function(value, key) {
  # value <- test_val("20 01 00 00")
  # key <- subset(PARAMETERS, Label == "POSITIVE_G_OFFSET_Y")
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_positive_g_offset_z <- function(value, key) {
  # value <- test_val("24 01 00 00")
  # key <- subset(PARAMETERS, Label == "POSITIVE_G_OFFSET_Z")
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_sample_rate <- function(value, key) {
  # value <- test_val("1E 00 00 00")
  # key <- subset(PARAMETERS, Label == "SAMPLE_RATE")
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_zero_g_offset_x <- function(value, key) {
  # value <- test_val("44 00 00 00")
  # key <- subset(PARAMETERS, Label == "ZERO_G_OFFSET_X")
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_zero_g_offset_y <- function(value, key) {
  # value <- test_val("1F 00 00 00")
  # key <- subset(PARAMETERS, Label == toupper("zero_g_offset_y"))
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_zero_g_offset_z <- function(value, key) {
  # value <- test_val("3F 00 00 00")
  # key <- subset(PARAMETERS, Label == toupper("zero_g_offset_z"))
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_imu_negative_g_offset_x <- function(value, key) {
  # value <- test_val("1E F8 FF FF")
  # key <- subset(PARAMETERS, Label == toupper("imu_negative_g_offset_x"))
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_imu_negative_g_offset_y <- function(value, key) {
  # value <- test_val("A7 F7 FF FF")
  # key <- subset(PARAMETERS, Label == toupper("imu_negative_g_offset_y"))
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_imu_negative_g_offset_z <- function(value, key) {
  # value <- test_val("DC F7 FF FF")
  # key <- subset(PARAMETERS, Label == toupper("imu_negative_g_offset_z"))
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_imu_positive_g_offset_x <- function(value, key) {
  # value <- test_val("1D 08 00 00")
  # key <- subset(PARAMETERS, Label == toupper("imu_positive_g_offset_x"))
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_imu_positive_g_offset_y <- function(value, key) {
  # value <- test_val("A4 07 00 00")
  # key <- subset(PARAMETERS, Label == toupper("imu_positive_g_offset_y"))
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_imu_positive_g_offset_z <- function(value, key) {
  # value <- test_val("00 08 00 00")
  # key <- subset(PARAMETERS, Label == toupper("imu_positive_g_offset_z"))
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_utc_offset <- function(value, key) {
  # value <- test_val("00 00 00 00")
  # key <- subset(PARAMETERS, Label == toupper("utc_offset"))
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_imu_zero_g_offset_x <- function(value, key) {
  # value <- test_val("00 00 00 00")
  # key <- subset(PARAMETERS, Label == toupper("imu_zero_g_offset_x"))
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_imu_zero_g_offset_y <- function(value, key) {
  # value <- test_val("FE FF FF FF")
  # key <- subset(PARAMETERS, Label == toupper("imu_zero_g_offset_y"))
  par_battery_voltage(value, key)
}

#' @rdname par_battery_voltage
par_imu_zero_g_offset_z <- function(value, key) {
  # value <- test_val("37 00 00 00")
  # key <- subset(PARAMETERS, Label == toupper("imu_zero_g_offset_z"))
  par_battery_voltage(value, key)
}

# Alias for `par_firmware_version` ----------------------------------------

#' @rdname par_firmware_version
par_wireless_firmware_version <- function(value, key) {
  # value <- test_val("01 00 01 01")
  # key <- subset(PARAMETERS, Label == "WIRELESS_FIRMWARE_VERSION")
  par_firmware_version(value, key)
}

# Alias for `par_calibration_time` ----------------------------------------

#' @rdname par_calibration_time
par_target_start_time <- function(value, key, tz) {
  # value <- test_val("E0 25 D2 54")
  # key <- subset(PARAMETERS, Label == "TARGET_START_TIME")
  par_calibration_time(value, key, tz)
}

#' @rdname par_calibration_time
par_target_stop_time <- function(value, key, tz) {
  # value <- test_val("70 85 D3 54")
  # key <- subset(PARAMETERS, Label == "TARGET_STOP_TIME")
  
  #NOTE: Documentation says this is supposed to return 15:04:16, but it returns
  #15:04:00. I think it's a documentation error, as `par_calibration_time` gives
  #what it's supposed to for all the other examples
  
  par_calibration_time(value, key, tz)
}

#' @rdname par_calibration_time
par_time_of_day <- function(value, key, tz) {
  # value <- test_val("F2 24 D2 54")
  # key <- subset(PARAMETERS, Label == "TIME_OF_DAY")
  par_calibration_time(value, key, tz)
}

#' @rdname par_calibration_time
# Alias for `par_wireless_serial_number` ----------------------------------

#' @rdname par_wireless_serial_number
par_hrm_serial_number_h <- function(value, key) {
  # value <- test_val("00 00 00 00")
  # key <- subset(PARAMETERS, Label == toupper("hrm_serial_number_h"))
  par_wireless_serial_number(value, key)
}

#' @rdname par_wireless_serial_number
par_hrm_serial_number_l <- function(value, key) {
  # value <- test_val("00 00 00 00")
  # key <- subset(PARAMETERS, Label == toupper("hrm_serial_number_l"))
  par_wireless_serial_number(value, key)
}

#' @rdname par_wireless_serial_number
par_proximity_interval <- function(value, key) {
  # value <- test_val("60 EA 00 00")
  # key <- subset(PARAMETERS, Label == toupper("proximity_interval"))
  # NOTE: Online documentation says this should be 59910 milliseconds, and that
  # it will usually be 60000 in practice (i.e., 60 seconds). But I get 60
  # seconds using the test value anyway, not 59.910...
  par_wireless_serial_number(value, key)
}
