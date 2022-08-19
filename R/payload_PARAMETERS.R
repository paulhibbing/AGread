process_parameters <- function(payload_chunk, tz) {

  address <- readBin(payload_chunk[1:2], "integer", 2, 2, FALSE)
  identifier <- readBin(payload_chunk[3:4], "integer", 2, 2, FALSE)
  value <- payload_chunk[5:8]

  match_index <- paste(address, identifier) ==
    paste(PARAMETERS$Address.Space, PARAMETERS$Identifier)

  if (!any(match_index)) {
    key <- PARAMETERS[1, ]
    key$Address.Space <- address
    key$Identifier <- identifier
    key$Label <- "UNUSED KEY"
    key[ ,c("Type", "Range", "Units", "Description")] <- NA
    key$value <- NA
    return(key)
  }

  key <- PARAMETERS[match_index, ]
  stopifnot(length(unique(key$Label)) == 1)
  label <- unique(key$Label)

  value <- switch(
    label,
    "BATTERY_STATE" = par_battery_state(value, key),
    "BATTERY_VOLTAGE" = par_battery_voltage(value, key),
    "BOARD_REVISION" = par_board_revision(value, key),
    "CALIBRATION_TIME" = par_calibration_time(value, key, tz),
    "FIRMWARE_VERSION" = par_firmware_version(value, key),
    "MEMORY_SIZE" = par_memory_size(value, key),
    "FEATURE_CAPABILITIES" = par_feature_capabilities(value, key),
    "DISPLAY_CAPABILITIES" = par_display_capabilities(value, key),
    "WIRELESS_FIRMWARE_VERSION" = par_wireless_firmware_version(value, key),
    "IMU_ACCEL_SCALE" = par_imu_accel_scale(value, key),
    "IMU_GYRO_SCALE" = par_imu_gyro_scale(value, key),
    "IMU_MAG_SCALE" = par_imu_mag_scale(value, key),
    "ACCEL_SCALE" = par_accel_scale(value, key),
    "IMU_TEMP_SCALE" = par_imu_temp_scale(value, key),
    "IMU_TEMP_OFFSET" = par_imu_temp_offset(value, key),
    "WIRELESS_MODE" = par_wireless_mode(value, key),
    "WIRELESS_SERIAL_NUMBER" = par_wireless_serial_number(value, key),
    "FEATURE_ENABLE" = par_feature_enable(value, key),
    "DISPLAY_CONFIGURATION" = par_display_configuration(value, key),
    "NEGATIVE_G_OFFSET_X" = par_negative_g_offset_x(value, key),
    "NEGATIVE_G_OFFSET_Y" = par_negative_g_offset_y(value, key),
    "NEGATIVE_G_OFFSET_Z" = par_negative_g_offset_z(value, key),
    "POSITIVE_G_OFFSET_X" = par_positive_g_offset_x(value, key),
    "POSITIVE_G_OFFSET_Y" = par_positive_g_offset_y(value, key),
    "POSITIVE_G_OFFSET_Z" = par_positive_g_offset_z(value, key),
    "SAMPLE_RATE" = par_sample_rate(value, key),
    "TARGET_START_TIME" = par_target_start_time(value, key, tz),
    "TARGET_STOP_TIME" = par_target_stop_time(value, key, tz),
    "TIME_OF_DAY" = par_time_of_day(value, key, tz),
    "ZERO_G_OFFSET_X" = par_zero_g_offset_x(value, key),
    "ZERO_G_OFFSET_Y" = par_zero_g_offset_y(value, key),
    "ZERO_G_OFFSET_Z" = par_zero_g_offset_z(value, key),
    "HRM_SERIAL_NUMBER_H" = par_hrm_serial_number_h(value, key),
    "HRM_SERIAL_NUMBER_L" = par_hrm_serial_number_l(value, key),
    "PROXIMITY_INTERVAL" = par_proximity_interval(value, key),
    "IMU_NEGATIVE_G_OFFSET_X" = par_imu_negative_g_offset_x(value, key),
    "IMU_NEGATIVE_G_OFFSET_Y" = par_imu_negative_g_offset_y(value, key),
    "IMU_NEGATIVE_G_OFFSET_Z" = par_imu_negative_g_offset_z(value, key),
    "IMU_POSITIVE_G_OFFSET_X" = par_imu_positive_g_offset_x(value, key),
    "IMU_POSITIVE_G_OFFSET_Y" = par_imu_positive_g_offset_y(value, key),
    "IMU_POSITIVE_G_OFFSET_Z" = par_imu_positive_g_offset_z(value, key),
    "UTC_OFFSET" = par_utc_offset(value, key),
    "IMU_ZERO_G_OFFSET_X" = par_imu_zero_g_offset_x(value, key),
    "IMU_ZERO_G_OFFSET_Y" = par_imu_zero_g_offset_y(value, key),
    "IMU_ZERO_G_OFFSET_Z" = par_imu_zero_g_offset_z(value, key),
    "SENSOR_CONFIGURATION" = par_sensor_configuration(value, key)
  )

  return(value)

}
