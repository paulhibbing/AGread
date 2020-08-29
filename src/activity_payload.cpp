#include <Rcpp.h>
using namespace Rcpp;

//' Extract a 12-bit integer from two bytes
//'
//' @param x the bytes (RawVector) from which to extract the integer
//' @param i1 integer. The index of the first byte
//' @param i2 integer. The index of the second byte
//' @param full_first boolean. Use the full first byte and half of the second?
//'
//' @keywords internal
// [[Rcpp::export]]
int get_int12(RawVector x, int i1, int i2, bool full_first) {
  int value;
  if (full_first) {
    value = (x[i1] << 4) + ((x[i2] & 0xF0) >> 4);
  } else {
    value = ((x[i1] & 0x0F) << 8) + x[i2];
  }
  if (value > 2047) {
    value |= 0xF000;
  }
  return value;
}

//' Parse the payload for an ACTIVITY packet
//' @keywords internal
// [[Rcpp::export]]
DataFrame activity_payload(
    RawVector payload, int samp_rate,
    int scale_factor, bool is_last_packet
) {

  //Test for last packet, and deal accordingly
  bool length_2 = (payload.size() % 2 == 0);
  bool length_3 = (payload.size() % 3 == 0);
  LogicalVector size_tests = LogicalVector::create(
    length_2, length_3
  );

  bool test_pass = is_true(all(size_tests));
  bool last_quit = (is_last_packet & (!test_pass));

  if (last_quit) {
    List result = List::create(R_NilValue);
    return result;
  }

  //Otherwise move on
  if (!test_pass) {
    stop("Payload has unexpected length and is not the last packet");
  }

  bool is_signed = TRUE;
  DoubleVector accel_x(samp_rate);
  DoubleVector accel_y(samp_rate);
  DoubleVector accel_z(samp_rate);

  int counter = 0;
  for (int i = 0; i < (payload.size() - 1); i += 2) {
    int int_result = get_int12(payload, i + 1, i, is_signed);
    double dub_result = double(int_result) / scale_factor;
    int col = i % 3;
    if (col == 0) {
      accel_x[counter] = dub_result;
    } else if (col == 2) {
      accel_y[counter] = dub_result;
    } else if (col == 1) {
      accel_z[counter] = dub_result;
      counter += 1;
    }
  }

  List final_result = List::create(
    Named("Accelerometer_X") = accel_x,
    Named("Accelerometer_Y") = accel_y,
    Named("Accelerometer_Z") = accel_z
  );

  return final_result;

}
