#ifndef IMU_PAYLOAD_H
#define IMU_PAYLOAD_H

#include <Rcpp.h>

Rcpp::DataFrame legacy_payload_parse_sensor_data_25C(
    Rcpp::RawVector payload, Rcpp::DataFrame info,
    int id, int samp_rate, Rcpp::Datetime timestamp
);

Rcpp::List dev_payload_parse_sensor_data_25C(
    Rcpp::RawVector payload, Rcpp::DataFrame info,
    int id, int samp_rate
);

#endif
