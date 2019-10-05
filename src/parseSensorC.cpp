#include <Rcpp.h>
#include "helpers.h"
#include "interpolate.h"
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

  CharacterVector frame_names = input.names();

  for (int i = 0; i < input.size(); ++i) {
    NumericVector test = input[i];
    if(test.size() <= 1 || frame_names[i] == "Discard") {
      input.erase(i);
      frame_names.erase(i);
    }
  }

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
//' @param samp_rate integer. The IMU sampling rate.
//' @param timestamp Datetime. The packet timestamp
//'
//' @keywords internal
// [[Rcpp::export]]
DataFrame payload_parse_sensor_data_25C(
    RawVector payload, DataFrame info,
    int id, int samp_rate, Datetime timestamp
) {

  // Setup, check payload id, and get timestamps

    check_id(payload, id);

    DatetimeVector times(samp_rate);
    for (int ts = 0; ts < samp_rate; ++ts) {
      double ts_frac = ts / double(samp_rate);
      times[ts] = timestamp + ts_frac;
    }

  // Pull apart the schema payload into separate vectors for use
  // in future for loop(s)

    LogicalVector endians = info["is_big_endian"];
    LogicalVector signs = info["is_signed"];
    CharacterVector labels = info["label"];
    NumericVector scales = info["scale_factor"];
    IntegerVector col_sizes = info["n_bytes"];
    IntegerVector orig_offsets = info["offset_bytes"];
    IntegerVector offsets = clone(orig_offsets);
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

    List result(labels.size() + 1);
      result[0] = times;

    CharacterVector names(result.size());
      names[0] = "Timestamp";
      for (int i = 1; i < names.size(); ++i) {
        names[i] = labels[i-1];
      }
      result.names() = names;

  // Loop over the columns and calculate the values

    for (int i = 0; i < n_cols; ++i) {

      IntegerVector values = int(n_samples);

      double scale = scales[i];
      int size = col_sizes[i];
      bool size2 = size == 2;
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

          values[j] = new_value;

        }

      NumericVector scaled_values(values.size());

      bool scale_zero = scale == 0;
      if (!scale_zero) {
        for (int k = 0; k < values.size(); ++k) {
          scaled_values[k] = values[k] / scale;
        }
      }

      result[i + 1] = interpolate_IMU(
        scaled_values, samp_rate
      );

    }

    DataFrame df_result = imu_df(result);

    return df_result;

}
