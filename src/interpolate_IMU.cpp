#include <Rcpp.h>
using namespace Rcpp;

//' @rdname sensor_resample
//' @aliases Resample Interpolate
//' @keywords internal
// [[Rcpp::export]]
NumericVector interpolate_IMU(
    NumericVector original_samples, int target_frequency
) {
  NumericVector new_values(0);
  double interval = original_samples.size() / double(target_frequency);
  for (int i = 0; i < target_frequency; ++i) {
    int index = floor(i * interval);
    double p = original_samples[index];
    double n;
    if (index + 1 < original_samples.size()) {
      n = original_samples[index + 1];
    } else {
      n = p;
    }
    double a = (i * interval) - index;
    double new_value = ((1 - a) * p) + (a * n);
    new_values.push_back(new_value);
  }
  return new_values;
}
