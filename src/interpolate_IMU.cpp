#include <Rcpp.h>
using namespace Rcpp;

//' Interpolate an IMU sensor column to the expected sampling frequency
//'
//' @param samples NumericVector. The original stream of sensor samples
//' @param target_freq integer. The sampling frequency, in Hz
//'
//' @keywords internal
// [[Rcpp::export]]
NumericVector interpolate_IMU(NumericVector samples, int target_freq) {
  NumericVector new_values(0);
  double interval = samples.size() / double(target_freq);
  for (int i = 0; i < target_freq; ++i) {
    int index = floor(i * interval);
    double p = samples[index];
    double n;
    if (index + 1 < samples.size()) {
      n = samples[index + 1];
    } else {
      n = p;
    }
    double a = (i * interval) - index;
    double new_value = ((1 - a) * p) + (a * n);
    new_values.push_back(new_value);
  }
  return new_values;
}
