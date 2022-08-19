## These functions only exist to make code clearer. Not all PARAMETERS records
## require a unique decoding process, but it's easier to follow if there's a
## decoding function whose name matches the record label


# Alias for `par_battery_voltage` -----------------------------------------

par_board_revision  <-
par_imu_accel_scale <-
par_imu_gyro_scale  <-
par_imu_mag_scale   <-
par_accel_scale     <-
par_imu_temp_scale  <-
par_imu_temp_offset <-
par_negative_g_offset_x <-
par_negative_g_offset_y <-
par_negative_g_offset_z <-
par_positive_g_offset_x <-
par_positive_g_offset_y <-
par_positive_g_offset_z <-
par_sample_rate     <-
par_zero_g_offset_x <-
par_zero_g_offset_y <-
par_zero_g_offset_z <-
par_imu_negative_g_offset_x <-
par_imu_negative_g_offset_y <-
par_imu_negative_g_offset_z <-
par_imu_positive_g_offset_x <-
par_imu_positive_g_offset_y <-
par_imu_positive_g_offset_z <-
par_utc_offset          <-
par_imu_zero_g_offset_x <-
par_imu_zero_g_offset_y <-
par_imu_zero_g_offset_z <- function(value, key) {

  par_battery_voltage(value, key)

}


# Alias for `par_firmware_version` ----------------------------------------

par_wireless_firmware_version <- function(value, key) {

  par_firmware_version(value, key)

}


# Alias for `par_calibration_time` ----------------------------------------

par_target_start_time <-
par_target_stop_time  <-
par_time_of_day       <- function(value, key, tz) {

  par_calibration_time(value, key, tz)

}


# Alias for `par_wireless_serial_number` ----------------------------------

par_hrm_serial_number_h <-
par_hrm_serial_number_l <-
par_proximity_interval  <- function(value, key) {

  par_wireless_serial_number(value, key)

}
