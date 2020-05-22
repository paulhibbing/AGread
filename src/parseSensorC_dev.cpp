#include <Rcpp.h>
#include "helpers.h"
#include "interpolate.h"
using namespace Rcpp;

//' @rdname legacy_payload_parse_sensor_data_25C
//' @keywords internal
// [[Rcpp::export]]
List dev_payload_parse_sensor_data_25C(
    RawVector payload, DataFrame info,
    int id, int samp_rate
) {

  check_id(payload, id);

  // Pull apart the schema payload into separate vectors for use
  // in the loops

    LogicalVector endians = info["is_big_endian"];
    LogicalVector signs = info["is_signed"];
    CharacterVector labels = info["label"];
    NumericVector scales = info["scale_factor"];
    IntegerVector col_sizes = info["n_bytes"];
    IntegerVector orig_offsets = info["offset_bytes"];
    IntegerVector offsets = clone(orig_offsets);
    int n_cols = info.nrow();

  // Identify record offset (i.e., number of bytes per sample)

    int record_offset = 0;
    for (int i = 0; i < col_sizes.length(); ++i) {
      record_offset += col_sizes[i];
    }

  // Identify the number of samples (can't use sample rate since not all packets
  // hit that target, meaning they need interpolation)

    int n_samples = payload.size();
    n_samples -= 2;
    n_samples /= record_offset;

  // Initialize the packet

    List result(labels.size());
    result.names() = labels;

  // Declare variables for outer loop (column by column)

    double scale;
    int size;
    bool size2;
    bool big_endian;
    bool is_signed;
    int offset;

  // Declare variables for inner loop (sample by sample, or row by row)

    int i1;
    int i2;
    int new_value;

  // Outer loop (column by column)

  for (int i = 0; i < n_cols; ++i) {

    IntegerVector values = int(n_samples);

    scale = scales[i];
    size = col_sizes[i];
    size2 = size == 2;
    big_endian = endians[i];
    is_signed = signs[i];
    offset = offsets[i] + 2;

    // Begin inner loop (sample by sample, or row by row)

    for (int j = 0; j < n_samples; ++j) {

      i1 = offset + (j * record_offset);
      i2 = i1 + 1;
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

    // End inner loop

    NumericVector scaled_values(values.size());

    bool scale_zero = scale == 0;
    if (!scale_zero) {
      for (int k = 0; k < values.size(); ++k) {
        scaled_values[k] = values[k] / scale;
      }
    }

    result[i] = interpolate_IMU(
      scaled_values, samp_rate
    );

  }

  return result;

}
