#include <Rcpp.h>
using namespace Rcpp;

//' @rdname impute_primary
//' @param vector_size int. The size of the final vector
//' @param accel_input NumericVector. The acceleromter values to reference for
//'   latching
//' @param samp_rate int. The sampling rate
//' @keywords internal
// [[Rcpp::export]]
NumericVector latch_accel(
    int vector_size,
    NumericVector accel_input,
    int samp_rate
) {
  NumericVector result(vector_size);
  for (int i = 0; i < accel_input.size(); ++i) {
    for (int j = 0; j < samp_rate; j++) {
      int new_index = (i * samp_rate) + j;
      result[new_index] = accel_input[i];
    }
  }
  return result;
}

//' @rdname impute_primary
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

//' @rdname impute_primary
//' @param indices IntegerVector containing latch indices
//' @param RAW DataFrame containing raw acceleration data
//' @keywords internal
// [[Rcpp::export]]
DataFrame get_latch_values(
    IntegerVector indices,
    DataFrame RAW
) {

  CharacterVector variables = CharacterVector::create(
    "Accelerometer_X",
    "Accelerometer_Y",
    "Accelerometer_Z"
  );

  DataFrame result = DataFrame::create(
    Named("latch_index") = indices,
    Named("Accelerometer_X") = NumericVector(indices.size()),
    Named("Accelerometer_Y") = NumericVector(indices.size()),
    Named("Accelerometer_Z") = NumericVector(indices.size())
  );

  for (int i = 0; i < variables.size(); ++i) {

    NumericVector new_vals(indices.size());
    String var_name(variables[i]);
    NumericVector ref_vals = RAW[var_name];

    for (int j = 0; j < indices.size(); ++j) {
      new_vals[j] = ref_vals[indices[j] - 1];
    }

    result[var_name] = new_vals;

  }

  return result;

}

//' @rdname impute_primary
//' @param timestamps vetor of timestamps on which to perform latching
//' @param accel_x vector of x-axis accelerations on which to perform latching
//' @param accel_y vector of y-axis accelerations on which to perform latching
//' @param accel_z vector of z-axis accelerations on which to perform latching
//' @param return_empty bool. Return an empty data frame?
//' @keywords internal
// [[Rcpp::export]]
DataFrame get_latch_entries(
    int samp_rate,
    DatetimeVector timestamps,
    NumericVector accel_x,
    NumericVector accel_y,
    NumericVector accel_z,
    bool return_empty = false
) {

  if (return_empty) {
    DatetimeVector empty_times(0);
    NumericVector x_vals(0);
    NumericVector y_vals(0);
    NumericVector z_vals(0);
    DataFrame frame_result = DataFrame::create(
      Named("Timestamp") = empty_times,
      Named("Accelerometer_X") = x_vals,
      Named("Accelerometer_Y") = y_vals,
      Named("Accelerometer_Z") = z_vals
    );
    return frame_result;
  }

  int final_size = timestamps.size() * samp_rate;

  DatetimeVector all_times(final_size);
  for (int i = 0; i < timestamps.size(); ++i) {
    for (int j = 0; j < samp_rate; j++) {
      double frac = double(j)/double(samp_rate);
      Datetime new_time = timestamps[i] + frac;
      int new_index = (i * samp_rate) + j;
      all_times[new_index] = new_time;
    }
  }

  NumericVector all_x = latch_accel(
    final_size, accel_x, samp_rate
  );
  NumericVector all_y = latch_accel(
    final_size, accel_y, samp_rate
  );
  NumericVector all_z = latch_accel(
    final_size, accel_z, samp_rate
  );

  DataFrame frame_result = DataFrame::create(
    Named("Timestamp") = all_times,
    Named("Accelerometer_X") = all_x,
    Named("Accelerometer_Y") = all_y,
    Named("Accelerometer_Z") = all_z
  );

  return frame_result;

}

//' @rdname impute_primary
//' @keywords internal
// [[Rcpp::export]]
DataFrame latch_replicate(
    Datetime start_time, Datetime stop_time,
    double x_val, double y_val, double z_val
) {

  int n = stop_time - start_time + 1;

  DatetimeVector timestamps(n);
  for (int i = 0; i < n; ++i) {
    timestamps[i] = start_time + i;
  }

  NumericVector accel_x(n, x_val);
  NumericVector accel_y(n, y_val);
  NumericVector accel_z(n, z_val);

  DataFrame result = DataFrame::create(
    Named("Timestamp") = timestamps,
    Named("Accelerometer_X") = accel_x,
    Named("Accelerometer_Y") = accel_y,
    Named("Accelerometer_Z") = accel_z
  );

  return result;

}
