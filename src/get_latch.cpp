#include <Rcpp.h>
using namespace Rcpp;

//' @rdname check_gaps
//' @param missing_times vector of missing timestamps for which to identify a
//'   latch index
//' @param reference_times vector of reference timestamps for use in determining
//'   the latch index
//' @keywords internal
// [[Rcpp::export]]
IntegerVector get_latch_index(
    DatetimeVector missing_times,
    DatetimeVector reference_times
) {

  IntegerVector indices(missing_times.size());
  int j = 0;

  for (int i = 0; i < indices.size(); ++i) {
    while (missing_times[i] > reference_times[j]) {
      j++;
      if (j>=reference_times.size()) {
        break;
      }
    }
    indices[i] = j;
  }

  return indices;

}

//' @rdname check_gaps
//' @param sleeps DataFrame containing idle sleep mode information
//' @param RAW DataFrame containing raw acceleration data
// [[Rcpp::export]]
DataFrame get_latch_values(
    DataFrame sleeps, DataFrame RAW
) {

  IntegerVector indices = sleeps["latch_index"];

  CharacterVector variables = CharacterVector::create(
    "Accelerometer_X",
    "Accelerometer_Y",
    "Accelerometer_Z"
  );

  for (int i = 0; i < variables.size(); ++i) {

    NumericVector new_vals(0);
    String var_name(variables[i]);
    NumericVector ref_vals = RAW[var_name];

    for (int j = 0; j < indices.size(); ++j) {
      new_vals.push_back(ref_vals[indices[j]]);
    }

    sleeps.push_back(new_vals, var_name);

  }

  return sleeps;

}
