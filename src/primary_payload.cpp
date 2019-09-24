#include <Rcpp.h>
#include "helpers.h"
using namespace Rcpp;

//' Parse a packet of primary accelerometer data in C++
//'
//' @param payload RawVector containing the payload bytes
//' @param is_last_packet logical. Is this the last packet in the file?
//' @param samp_rate integer reflecting the sampling rate
//' @param scale_factor integer reflecting the scale factor
//'
//' @keywords internal
// [[Rcpp::export]]
DataFrame payload_parse_activity2_26C(
    RawVector payload, int samp_rate,
    int scale_factor, bool is_last_packet
) {

  //Test for last packet
  bool length_2 = (payload.size() % 2 == 0);
  bool length_3 = (payload.size() % 3 == 0);
  LogicalVector size_tests(0);
  size_tests.push_back(length_2);
  size_tests.push_back(length_3);
  bool test_pass = is_true(all(size_tests));

  //Deal with USB event if thats what the packet indicates
  int usb_event = payload.size() == 1;
  if (usb_event) {

    NumericVector miss_vec(0);
    for (int i = 0; i < samp_rate; ++i) {
      miss_vec.push_back(0);
    }

    DataFrame result = DataFrame::create(
      Named("Accelerometer_X") = miss_vec,
      Named("Accelerometer_Y") = miss_vec,
      Named("Accelerometer_Z") = miss_vec
    );

    return result;

  }

  //If last packet doesnt meet expectations, quit
  bool last_quit = (is_last_packet & (!test_pass));
  if (last_quit) {
    //double empty_val = R_NilValue;
    DataFrame result = DataFrame::create(
      Named("Accelerometer_X") = R_NilValue,
      Named("Accelerometer_Y") = R_NilValue,
      Named("Accelerometer_Z") = R_NilValue
    );
    return result;
  }

  //Otherwise move on
  if (!test_pass) {
    stop("Payload has unexpected length and is not the last packet");
  }

  bool is_signed = TRUE;
  DoubleVector accel_x(0);
  DoubleVector accel_y(0);
  DoubleVector accel_z(0);

  for (int i = 0; i < (payload.size() - 1); i += 2) {
    int int_result = get_short(payload, i + 1, i, is_signed);
    double dub_result(int_result);
    double scaled_result = mid_round(
      dub_result / scale_factor, 3
    );
    int col = i % 3;
    if (col == 0) {
      accel_x.push_back(scaled_result);
    } else if (col == 2) {
      accel_y.push_back(scaled_result);
    } else if (col == 1) {
      accel_z.push_back(scaled_result);
    }
  }

  DataFrame final_result = DataFrame::create(
    Named("Accelerometer_X") = accel_x,
    Named("Accelerometer_Y") = accel_y,
    Named("Accelerometer_Z") = accel_z
  );

  return final_result;

}
