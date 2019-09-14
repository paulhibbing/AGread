#include <Rcpp.h>
#include "helpers.h"
using namespace Rcpp;

//' Check sensor payload ID prior to parsing the packet
//'
//' @param x the raw payload
//' @param id integer. The \code{SENSOR_SCHEMA} id
//'
//' @keywords internal
// [[Rcpp::export]]
void check_id(RawVector x, int id) {
  int payload_id = int(x[1] << 8 | x[0]);
  bool id_check = payload_id == id;
  try {
    if (id_check) {
      return;
    } else {
      throw "Payload id does not equal schema id";
    }
  } catch(const char* msg) {
    ::Rf_error(msg);
  }
}

//' Convert parsed packet data from list to data frame
//'
//' @param input parsed packet data (as list)
//'
//' @keywords internal
// [[Rcpp::export]]
DataFrame imu_df(List input){

  for (int i = 0; i < input.length(); ++i) {
    NumericVector test = input[i];
    if(test.size() == 0) input.erase(i);
  }

  CharacterVector frame_names = input.names();
  DataFrame res(input);
  res.names() = frame_names;
  return res;

}

//' Parse SENSOR_DATA packet in c++
//'
//' @param payload raw vector of payload bytes
//' @param info \code{sensorColumns} information from a \code{SENSOR_SCHEMA}
//'   object
//' @param id integer. The \code{id} information from a \code{SENSOR_SCHEMA}
//'   object
//'
//' @keywords internal
// [[Rcpp::export]]
DataFrame payload_parse_sensor_data_25C(
    RawVector payload, DataFrame info, int id
) {

  // Setup and check payload id
  check_id(payload, id);

  // Pull apart the schema payload into separate vectors for use
  // in future for loop(s)
  LogicalVector endians = info["is_big_endian"];
  LogicalVector signs = info["is_signed"];
  CharacterVector labels = info["label"];
  DoubleVector scales = info["scale_factor"];
  IntegerVector col_sizes = info["n_bytes"];
  IntegerVector orig_offsets = info["offset_bytes"];
  IntegerVector offsets = clone(orig_offsets);
  // int samp_rate = schema_payload["samples"];
  int n_cols = info.nrow();

  // Identify the length of each total record
  int record_offset = 0;
  for (int i = 0; i < col_sizes.length(); ++i) {
    record_offset += col_sizes[i];
  }

  // Identify the number of samples
  int n_samples = payload.size();
  n_samples -= 2;
  n_samples /= record_offset;

  // Set up the result data frame
  List result = List::create(
    Named("Dummy") = int(0)
  );

  // Loop over the columns and calculate the values
  for (int i = 0; i < n_cols; ++i) {

    IntegerVector values = int(0); // initialize a vector

    double scale = scales[i];
    int size = col_sizes[i];
    bool size2 = size == 2;
    String new_name(labels[i]);
    bool big_endian = endians[i];
    bool is_signed = signs[i];
    int offset = offsets[i] + 2;

    // Go row by row
    for (int j = 0; j < n_samples; ++j) {

      int i1 = offset + (j * record_offset);
      int i2 = i1 + 1;
      int new_value = 0;
      if (size2) {
        if (big_endian) {
          new_value = get_short(payload, i1, i2, is_signed);
        } else {
          new_value = get_short(payload, i2, i1, is_signed);
        }
      } else {
        if (is_signed) {
          new_value = int(payload[i1]);
        } else {
          new_value = (unsigned int)(payload[i1]);
        }
      }

      values.push_back(new_value);

    }

    //values.erase(0);

    DoubleVector scaled_values = 0;
    // scaled_values = as<DoubleVector>(scaled_values);
    bool scale_zero = scale == 0;
    if (!scale_zero) {
      for (int i = 0; i < values.length(); ++i) {
        // scaled_values[i] /= scale;
        scaled_values.push_back(values[i] / scale);
      }
    }

    result.push_back(scaled_values, new_name);

  }

  result.erase(0);
  DataFrame df_result = imu_df(result);

  return df_result;

}
