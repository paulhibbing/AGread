#include <Rcpp.h>
#include "helpers.h"
using namespace Rcpp;

//' Check sensor payload ID prior to parsing the packet
//'
//' @param x the raw payload
//' @param payload the sensor schema payload (as list) to compare against
//'
//' @keywords internal
// [[Rcpp::export]]
void check_id(RawVector x, List payload) {
  int id = int(x[1] << 8 | x[0]);
  bool id_check = id == int(payload["id"]);
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

//' Parse SENSOR_DATA packet in c++
//'
//' @inheritParams payload_parse_sensor_data_25
//'
//' @keywords internal
// [[Rcpp::export]]
List payload_parse_sensor_data_25C(RawVector payload, List schema) {

  // Setup and check payload id
  List schema_payload = schema["Payload"];
  check_id(payload, schema_payload);

  // Pull apart the schema payload into separate vectors for use
  // in future for loop(s)
  DataFrame info = schema_payload["sensorColumns"];
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

  return result;

}
