#include <Rcpp.h>
using namespace Rcpp;

//' @rdname impute_primary
//' @param gaps DataFrame with gap information
//' @keywords internal
// [[Rcpp::export]]
DataFrame impute_C(DataFrame gaps, DataFrame object) {

  CharacterVector accel_names = CharacterVector::create(
    "Accelerometer_X", "Accelerometer_Y", "Accelerometer_Z"
  );
  IntegerVector starts = gaps["start_index"];
  IntegerVector stops = gaps["stop_index"];

  for (int i = 0; i < accel_names.size(); ++i) {

    String col_name = accel_names[i];
    NumericVector accel_value = object(col_name);

    for (int j = 0; j < gaps.nrows(); ++j) {

      int start = starts[j];
      int stop = stops[j];
      int latch_index = start - 1;
      double latch_value = 0;
      if (latch_index >= 0) {
        latch_value = accel_value[latch_index];
      }

      for (int k = start; k <= stop; ++k) {
        accel_value[k] = latch_value;
      }

    }

    object(col_name) = accel_value;

  }

  return object;

}
