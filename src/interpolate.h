#ifndef INTERPOLATE_H
#define INTERPOLATE_H

#include <Rcpp.h>
Rcpp::NumericVector interpolate_C(
    Rcpp::NumericVector original_samples,
    int target_frequency
);
Rcpp::NumericVector interpolate_IMU(
    Rcpp::NumericVector samples,
    int target_freq
);
#endif
