#include <Rcpp.h>
using namespace Rcpp;

//' @rdname sensor_resample
//' @aliases Resample Interpolate
//' @keywords internal
// [[Rcpp::export]]
NumericVector interpolate_IMU(
    NumericVector original_samples, int target_frequency
) {

  double step_size = (
    original_samples.size() /
    double(target_frequency)
  );

  if (step_size == 1) {
    return original_samples;
  }

  NumericVector new_values(target_frequency);

  for (int i = 0; i < target_frequency; ++i) {

    double position = i * step_size;
    int index = floor(position);
    double step_fraction = position - index;

    double previous = original_samples[index];

    double next;
    if (index + 1 < original_samples.size()) {
      next = original_samples[index + 1];
    } else {
      next = previous;
    }

    new_values[i] = (
      (1 - step_fraction) * previous
    ) + (step_fraction * next);

  }

  return new_values;

}
